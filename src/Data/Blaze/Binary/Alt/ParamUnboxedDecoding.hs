{-# LANGUAGE UnboxedTuples, MagicHash, ScopedTypeVariables, BangPatterns, DeriveDataTypeable, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Blaze.Binary.Encoding
-- Copyright   : 2012, Simon Meier <iridcode@gmail.com>
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   :
-- Portability : portable
--
-- Decoding of binary values parametrized over the primitive parsers.
--
-- This version exploits a bunch of low-level tricks to improve the speed:
--   - it uses unboxed tuples to implement an IO monad that additionally
--     carries the pointer to the next byte to read.
--   - it stores that static content of the parsing state in the closure of
--     the primitive decoders. This way it only uses 'stg_ap_n' calls, which
--     are a lot faster than 'stg_ap_nv' calls, because 'stg_ap_nv' are
--     compiled to 'stg_ap_n' followed by 'stg_ap_v'.
--   - it uses exceptions to report failures. 
--   - it uses 'forkIOWithUnmask' to reserve its own stack for large inputs.
--
-- In some benchmarks this decoder is really fast. However, I must say that I
-- would not like to maintain this low-level hackery. Moreover, it does not
-- allow incremental input, as the parser cannot be suspended.
--
-----------------------------------------------------------------------------
module Data.Blaze.Binary.Alt.ParamUnboxedDecoding where

import Prelude hiding (catch)

import qualified Data.Blaze.Binary.Decoding as D

import Control.Applicative
import Control.Exception
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

import Data.Typeable
import qualified Data.ByteString.Internal as S
import GHC.Prim
import GHC.Ptr
import GHC.Word
import GHC.Exts
import GHC.IO (IO(IO))
import GHC.Conc.Sync (forkIOWithUnmask)
import Foreign 


------------------------------------------------------------------------------
-- Decoding exceptions
------------------------------------------------------------------------------

-- | Extract the 'Addr#' from a 'Ptr'.
{-# INLINE getPtr #-}
getPtr :: Ptr a -> Addr#
getPtr (Ptr p) = p

-- | Extract an 'IO' operation to its primtive representation.
{-# INLINE runIO #-}
runIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
runIO (IO io) = io

------------------------------------------------------------------------------
-- Decoding exceptions
------------------------------------------------------------------------------

-- | Internally, we use 'DecodingException' to report failed parses. This
-- allows us to write the succeeding parsing code, as if there would be no
-- failure. This works well, as we currently focus on parsing consecutive
-- chunks of memory. Note that copying the whole input once to make it
-- consecutive is very likely less effort than all the parameter copying
-- necessitated by having an interruptible parser.
data DecodingException = DecodingException String (Ptr Word8)
  deriving( Show, Typeable )

instance Exception DecodingException where

------------------------------------------------------------------------------
-- Primitive decoders
------------------------------------------------------------------------------

-- We currently use a boxed pointer because that results in the 'stg_ap_pv'
-- calling pattern, which is precompiled in contrast to the 'stg_ap_nv'
-- calling pattern.

-- Highly unsafe trick: primitive decoders are only called via an unknown call
-- and never inlined. Pattern mathcing on their result makes sure that their
-- evaluation took place. At least for the primitive ones that return unboxed
-- values.

type PrimDecoder a     = Ptr Word8 -> State# RealWorld -> (# State# RealWorld, Addr#, a       #)
type PrimDecoderWord   = Addr# -> (# Addr#, Word#   #)
type PrimDecoderInt    = Ptr Word8 -> State# RealWorld -> (# State# RealWorld, Addr#, Int#    #)
type PrimDecoderChar   = Addr# -> (# Addr#, Char#   #)
type PrimDecoderFloat  = Ptr Word8 -> State# RealWorld -> (# State# RealWorld, Addr#, Float#  #)
type PrimDecoderDouble = Ptr Word8 -> State# RealWorld -> (# State# RealWorld, Addr#, Double# #)

-- | These are the decoders for extracting primitive values. They are all
-- given
data PrimDecoders = PrimDecoders {
       pdWord8      :: PrimDecoderWord
     , pdWord16     :: PrimDecoderWord
     , pdWord32     :: PrimDecoderWord
     , pdWord64     :: PrimDecoderWord
     , pdInt8       :: PrimDecoderInt
     , pdInt16      :: PrimDecoderInt
     , pdInt32      :: PrimDecoderInt
     , pdInt64      :: PrimDecoderInt
     , pdFloat      :: PrimDecoderFloat
     , pdDouble     :: PrimDecoderDouble
     , pdChar       :: PrimDecoderChar
     , pdByteString :: PrimDecoder S.ByteString
     }

-- Prededefined primitive decoders
----------------------------------

-- FIXME: Make this code also works on big-endian and 32-bit machines.

{-# INLINE decodersLE #-}
decodersLE :: ForeignPtr Word8  -- ^ Pointer to the underlying buffer
           -> Ptr Word8         -- ^ Pointer to first byte after the buffer
           -> PrimDecoders
decodersLE !fpbuf !ipe = 
    PrimDecoders w8 w16 w32 w64
                 i8 i16 i32 i64
                 float double charUtf8 undefined
  where
    w8    = word (\ip -> fmap fromIntegral (peek (ip :: Ptr Word8)))
    w16   = word (\ip -> fmap fromIntegral (peek (ip :: Ptr Word16)))
    w32   = word (\ip -> fmap fromIntegral (peek (ip :: Ptr Word32)))
    w64   = word (\ip -> fmap fromIntegral (peek (ip :: Ptr Word64)))
    -- whost = word (\ip -> fmap truncateWord (peek (ip :: Ptr Word64)))

    i8    = int  (\ip -> fmap fromIntegral (peek (ip :: Ptr Int8)))
    i16   = int  (\ip -> fmap fromIntegral (peek (ip :: Ptr Int16)))
    i32   = int  (\ip -> fmap fromIntegral (peek (ip :: Ptr Int32)))
    i64   = int  (\ip -> fmap fromIntegral (peek (ip :: Ptr Int64)))
    -- ihost = int  (\ip -> fmap truncateInt  (peek (ip :: Ptr Int64)))

    -- FIXME: Implement for non-64bit arch
    -- truncateWord :: Word64 -> Word
    -- truncateWord = fromIntegral

    -- FIXME: Implement for non-64bit arch
    -- truncateInt :: Int64 -> Int
    -- truncateInt = fromIntegral

    {-# INLINE word #-}
    word :: forall a. Storable a => (Ptr a -> IO Word) -> PrimDecoderWord
    word peekAt = \ip0a -> 
      let ip0 = Ptr ip0a in
      case nextPtr (castPtr ip0 :: Ptr a) of
        ip1 | ip1 <= ipe -> case runIO (peekAt (castPtr ip0)) realWorld# of
                              (# _, W# x #) -> (# getPtr ip1, x #)
            | otherwise -> 
                case runIO (tooFewBytes ip0) realWorld# of
                  (# _, W# x #) -> (# getPtr ip0, x #)

    {-# INLINE int #-}
    int :: forall a. Storable a => (Ptr a -> IO Int) -> PrimDecoderInt
    int peekAt = \ip0 s0 -> 
      case nextPtr (castPtr ip0 :: Ptr a) of
        ip1 | ip1 <= ipe -> case runIO (peekAt (castPtr ip0)) s0 of
                              (# s1, I# x #) -> (# s1, getPtr ip1, x #)
            | otherwise -> 
                case runIO (tooFewBytes ip0) s0 of
                  (# s1, I# x #) -> (# s1, getPtr ip0, x #)
    
    {-# INLINE float #-}
    float :: PrimDecoderFloat
    float = \ip0 s0 -> 
      case nextPtr (castPtr ip0 :: Ptr Float) of
        ip1 | ip1 <= ipe -> case runIO (peek (castPtr ip0 :: Ptr Float)) s0 of
                              (# s1, F# x #) -> (# s1, getPtr ip1, x #)
            | otherwise -> 
                case runIO (tooFewBytes ip0) s0 of
                  (# s1, F# x #) -> (# s1, getPtr ip0, x #)
    
    {-# INLINE double #-}
    double :: PrimDecoderDouble
    double = \ip0 s0 -> 
      case nextPtr (castPtr ip0 :: Ptr Double) of
        ip1 | ip1 <= ipe -> case runIO (peek (castPtr ip0 :: Ptr Double)) s0 of
                              (# s1, D# x #) -> (# s1, getPtr ip1, x #)
            | otherwise -> 
                case runIO (tooFewBytes ip0) s0 of
                  (# s1, D# x #) -> (# s1, getPtr ip0, x #)
    
    {-# INLINE nextPtr #-}
    nextPtr :: forall a. Storable a => Ptr a -> Ptr Word8
    nextPtr = castPtr . (`plusPtr` (sizeOf (undefined :: a)))

    tooFewBytes :: Ptr Word8 -> IO a
    tooFewBytes = throw . DecodingException "too few bytes"
{-
    {-# INLINE wordN #-}
    wordN :: Int -> (Ptr Word8 -> PrimDecoderWord) -> PrimDecoderWord
    wordN n d = \ip0 s0 -> 
      case ip0 `plusPtr` n of
        ip1 | ip1 <= ipe -> d ip1 ip0 s0
            | otherwise -> 
                case runIO (throw (DecodingException "too few bytes" ip0)) s0 of
                  -- unreachable, but makes the type checker happy.
                  (# s1, W# w #) -> (# s1, getPtr ip0, w #)
-}

    charUtf8 :: PrimDecoderChar
    charUtf8 = \ip0a -> let ip0 = Ptr ip0a in
      if ip0 < ipe then
        case runIO (peek ip0) realWorld# of
          (# s1, w0 #) 
            | w0 < 0x80 -> (# getPtr (ip0 `plusPtr` 1), chr1 w0 #)

            | w0 < 0xe0 && ip0 `plusPtr` 2 <= ipe -> 
                case runIO (peekByteOff ip0 1) s1 of
                  (# s2, w1 #) ->
                    (# getPtr (ip0 `plusPtr` 2), chr2 w0 w1 #)

            | w0 < 0xf0 && ip0 `plusPtr` 3 <= ipe -> 
                case runIO (peekByteOff ip0 1) s1 of
                  (# s2, w1 #) -> case runIO (peekByteOff ip0 2) s2 of
                    (# s3, w2 #) ->
                      (# getPtr (ip0 `plusPtr` 3), chr3 w0 w1 w2 #)

            | ip0 `plusPtr` 4 <= ipe -> 
                case runIO (peekByteOff ip0 1) s1 of
                  (# s2, w1 #) -> case runIO (peekByteOff ip0 2) s2 of
                    (# s3, w2 #) -> case runIO (peekByteOff ip0 3) s3 of
                      (# s4, w3 #) -> 
                        let x = chr4 w0 w1 w2 w3 in 
                          if I# x <= 0x10ffff
                            then (# getPtr (ip0 `plusPtr` 4), chr# x #)
                            else runIOChr ip0 (throw (DecodingException ("invalid Unicode codepoint: " ++ show (I# x)) ip0))

            | otherwise -> runIOChr ip0 (tooFewBytes ip0)
        else runIOChr ip0 (tooFewBytes ip0)
      where
        runIOChr ip io = case runIO io realWorld# of
          (# s1, C# c #) -> (# getPtr ip, c #)


------------------------------------------------------------------------------
-- Decoder
------------------------------------------------------------------------------

-- | One decoding step. Note that we use a 'Ptr Word8' because the
-- 'stg_ap_pnv' calling patterns is not precompiled in GHC.
type DecodeStep a = 
          Ptr Word8                           -- ^ Next byte to read
       -> (# Addr#, a #)  
       -- ^ World state, new next byte to read, and decoded value

-- | A decoder for Haskell values.
newtype Decoder a = Decoder { 
          unDecoder :: PrimDecoders -> DecodeStep a
        }

-- Utilities
------------

-- | Convert an 'IO' action to a 'Decoder' action.
{-# INLINE ioToDecoder #-}
ioToDecoder :: IO a -> Decoder a
ioToDecoder (IO io) = Decoder $ \_ !(Ptr ip0) -> case io realWorld# of
    (# _, x #) -> (# ip0, x #)

-- | A 'DecodeStep' that fails with the given message.
failStep :: String -> DecodeStep a
failStep msg ip0 =
    case runIO (throw (DecodingException msg ip0)) realWorld# of
      -- unreachable, but makes the type checker happy.
      (# _, x #) -> (# getPtr ip0, x #)


-- Instances
------------

instance Functor Decoder where
    {-# INLINE fmap #-}
    fmap = \f (Decoder io) -> Decoder $ \pd ip0 -> 
        case io pd ip0 of
            (# ip1, x #) -> (# ip1, f x #)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure x = Decoder $ \_ ip0 -> (# getPtr ip0, x #)

    {-# INLINE (<*>) #-}
    Decoder fIO <*> Decoder xIO = Decoder $ \pd ip0 ->
        case fIO pd ip0 of
          (# ip1, f #) -> case xIO pd (Ptr ip1) of
            (# ip2, x #) -> (# ip2, f x #)

instance Monad Decoder where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    Decoder xIO >>= f = Decoder $ \pd ip0 ->
        case xIO pd ip0 of
          (# ip1, x #) -> unDecoder (f x) pd (Ptr ip1)

    {-# INLINE fail #-}
    fail = Decoder . const . failStep


-- Decoder execution
--------------------

-- | Execute a decoder on a strict 'S.ByteString'.
runDecoder :: Decoder a -> S.ByteString -> Either String a
runDecoder p (S.PS fpbuf off len) = S.inlinePerformIO $ do
    withForeignPtr fpbuf $ \pbuf -> do
        let !ip0 = pbuf `plusPtr` off
            !ipe = ip0 `plusPtr` len
            !pd  = decodersLE fpbuf ipe

            decodeFast = (handle (decodingException ip0)) $ do
                x <- IO $ \s0 -> case unDecoder p pd ip0 of
                                   (# _, x #) -> (# s0, x #)
                return (Right x)

            -- For deeply nested messages our decoder might overflow the
            -- stack. We report this error politely as a decoding failure
            -- instead of killing all the pure code above us. As a
            -- stackoverflow is an asynchronous exception, we have to ensure
            -- that we are not masked. Note that we are essentially allocating
            -- a fresh stack for the decoding using 'forkIOWithUnmask' :-)
            decodeLarge =  do
                mv <- newEmptyMVar
                _  <- forkIOWithUnmask $ \unmask ->
                          handle (allExceptions mv) $ 
                          handle (stackOverflow mv) $
                              unmask $ putMVar mv =<< (Right <$> decodeFast)
                res <- takeMVar mv
                case res of
                    Left e  -> throw e
                    Right x -> return x

        if len < 1024 then decodeFast else decodeLarge
  where
    decodingException :: Ptr Word8 -> DecodingException -> IO (Either String a)
    decodingException ip0 (DecodingException msg ip) = return $ Left $ 
        msg ++ 
        " (at byte " ++ show (ip `minusPtr` ip0) ++ " of " ++ show len ++ ")" 

    stackOverflow :: MVar (Either e (Either String a)) -> AsyncException -> IO ()
    stackOverflow mv StackOverflow = putMVar mv $ Right $ Left $ 
        "stack overflow: the message of size " ++ show len ++
        " may be nested too deeply."
    stackOverflow _ e = throw e

    allExceptions :: MVar (Either SomeException a) -> SomeException -> IO ()
    allExceptions mv e = putMVar mv (Left e)


-- Decoder construction
-----------------------


{-
requires :: Int -> Decoder a -> Decoder a
requires n p = Decoder $ \buf@(Buffer ip ipe) ->
    if ipe `minusPtr` ip >= n
      then unDecoder p buf
      else throw $ DecodingException $
             "required " ++ show n ++ 
             " bytes, but there are only " ++ show (ipe `minusPtr` ip) ++
             " bytes left."
-}

{-# INLINE prim #-}
prim :: b -> Decoder a
prim = error "PDecoder: prim - implement"
  {- sel = Decoder $ \pd fpbuf ip0 ipe s0 ->
     D.unDecoder (sel pd) fpbuf ip0 ipe s0  -}



-- Primitive parsers
--------------------

word8 :: Decoder Word8
word8 = Decoder $ \pd !(Ptr ip0) -> case pdWord8 pd ip0 of
                                      (# ip1, w #) -> (# ip1, W8# w #)

word8s = decodeList word8

string = decodeList char

-- {-# NOINLINE decodeList #-}
-- decodeList :: Decoder a -> Decoder [a]
-- decodeList x = go
--   where
--     go = do
--       tag <- word8
--       case tag of
--         0 -> return []
--         1 -> (:) <$> x <*> go
--         _ -> fail $ "decodeList: unexpected tag " ++ show tag
{-# NOINLINE decodeList #-}
decodeList :: Decoder a -> Decoder [a]
decodeList decode =
    int >>= go
  where
    go !n
      | n <= 0    = return []
      | otherwise = (:) <$> decode <*> go (n - 1)

{-# INLINE word16 #-}
word16 :: Decoder Word16
word16 = prim pdWord16

{-# INLINE word32 #-}
word32 :: Decoder Word32
word32 = prim pdWord32

{-# INLINE word64 #-}
word64 :: Decoder Word64
word64 = prim pdWord64

{-# INLINE word #-}
word :: Decoder Word
word = prim pdWord64

{-# INLINE int8 #-}
int8 :: Decoder Int8
int8 = prim pdInt8

{-# INLINE int16 #-}
int16 :: Decoder Int16
int16 = prim pdInt16

{-# INLINE int32 #-}
int32 :: Decoder Int32
int32 = prim pdInt32

{-# INLINE int64 #-}
int64 :: Decoder Int64
int64 = prim pdInt64

{-# INLINE int #-}
int :: Decoder Int
int = prim pdInt64

{-# INLINE float #-}
float :: Decoder Float
float = prim pdFloat

{-# INLINE double #-}
double :: Decoder Double
double = prim pdDouble

{-
{-# INLINE byteString #-}
byteString :: Int -> Decoder S.ByteString
byteString = \len -> prim (`pdByteString` len)
-}

char :: Decoder Char
char = Decoder $ \pd ip0 -> case pdChar pd (getPtr ip0) of
                              (# ip1, x #) -> (# ip1, C# x #)

{-# INLINE getAddr #-}
getAddr :: Ptr a -> Addr#
getAddr (Ptr a) = a

-- Decoder combinators
--------------------

{-
{-# INLINE decodeList #-}
decodeList :: Decoder a -> Decoder [a]
decodeList x = 
    go
  where 
    go = do tag <- word8
            case tag of
              0 -> return []
              1 -> (:) <$> x <*> go
              _ -> fail $ "decodeList: unexpected tag " ++ show tag
-}

{-# INLINE decodeMaybe #-}
decodeMaybe :: Decoder a -> Decoder (Maybe a)
decodeMaybe just = 
    go
  where 
    go = do tag <- word8
            case tag of
              0 -> return Nothing
              1 -> Just <$> just
              _ -> fail $ "decodeMaybe: unexpected tag " ++ show tag

{-# INLINE decodeEither #-}
decodeEither :: Decoder a -> Decoder b -> Decoder (Either a b)
decodeEither left right = 
    go
  where 
    go = do tag <- word8
            case tag of
              0 -> Left <$> left 
              1 -> Right <$> right
              _ -> fail $ "decodeEither: unexpected tag " ++ show tag


------------------------------------------------------------------------------
-- UTF-8 decoding helpers
------------------------------------------------------------------------------

chr1 :: Word8 -> Char#
chr1 (W8# x#) = chr# (word2Int# x#)
{-# INLINE chr1 #-}

chr2 :: Word8 -> Word8 -> Char#
chr2 (W8# x1#) (W8# x2#) = chr# (z1# +# z2#)
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
      !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3 :: Word8 -> Word8 -> Word8 -> Char#
chr3 (W8# x1#) (W8# x2#) (W8# x3#) = chr# (z1# +# z2# +# z3#)
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
      !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4             :: Word8 -> Word8 -> Word8 -> Word8 -> Int#
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
      (z1# +# z2# +# z3# +# z4#)
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !y4# = word2Int# x4#
      !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
      !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
      !z4# = y4# -# 0x80#
{-# INLINE chr4 #-}
