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
-----------------------------------------------------------------------------
module Data.Blaze.Binary.ParamDecoding where

import Prelude hiding (catch)

import qualified Data.Blaze.Binary.Decoding as D

import Control.Applicative
import Control.Exception

import Data.Typeable
import qualified Data.ByteString.Internal as S
import GHC.Prim
import GHC.Ptr
import GHC.Word
import GHC.Exts
import GHC.IO (IO(IO))
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

type PrimDecoder a     = Ptr Word8 -> State# RealWorld -> (# State# RealWorld, Addr#, a       #)
type PrimDecoderWord   = Ptr Word8 -> State# RealWorld -> (# State# RealWorld, Addr#, Word#   #)
type PrimDecoderInt    = Ptr Word8 -> State# RealWorld -> (# State# RealWorld, Addr#, Int#    #)
type PrimDecoderChar   = Ptr Word8 -> State# RealWorld -> (# State# RealWorld, Addr#, Char#   #)
type PrimDecoderFloat  = Ptr Word8 -> State# RealWorld -> (# State# RealWorld, Addr#, Float#  #)
type PrimDecoderDouble = Ptr Word8 -> State# RealWorld -> (# State# RealWorld, Addr#, Double# #)

-- | These are the decoders for extracting primitive values. They are all
-- given
data PrimDecoders = PrimDecoders {
       pdWord8      :: PrimDecoderWord
     , pdWord16     :: PrimDecoderWord
     , pdWord32     :: PrimDecoderWord
     , pdWord64     :: PrimDecoder Word64
     , pdWord       :: PrimDecoderWord
     , pdInt8       :: PrimDecoderInt
     , pdInt16      :: PrimDecoderInt
     , pdInt32      :: PrimDecoderInt
     , pdInt64      :: PrimDecoder Int64
     , pdInt        :: PrimDecoderInt
     , pdFloat      :: PrimDecoderFloat
     , pdDouble     :: PrimDecoderDouble
     , pdChar       :: PrimDecoderChar
     , pdByteString :: PrimDecoder S.ByteString
     }

-- Prededefined primitive decoders
----------------------------------

-- FIXME: Make this code also works on big-endian machines.

{-# INLINE decodersLE #-}
decodersLE :: ForeignPtr Word8  -- ^ Pointer to the underlying buffer
           -> Ptr Word8         -- ^ Pointer to first byte after the buffer
           -> PrimDecoders
decodersLE !fpbuf !ipe = 
    PrimDecoders w8
                 undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined
  where
    w8 ip0 s0 = case ip0 `plusPtr` sizeOf (undefined :: Word8) of
      ip1 | ip1 <= ipe -> case runIO (peek ip0) s0 of
                            (# s1, W8# w #) -> (# s1, getPtr ip1, w #)
          | otherwise -> case runIO (throw (DecodingException "too few bytes" ip0)) s0 of
                           -- unreachable, but makes the type checker happy.
                            (# s1, W8# w #) -> (# s1, getPtr ip0, w #)



------------------------------------------------------------------------------
-- Decoder
------------------------------------------------------------------------------

-- | One decoding step. Note that we use a 'Ptr Word8' because the
-- 'stg_ap_pnv' calling patterns is not precompiled in GHC.
type DecodeStep a = 
          Ptr Word8                         -- ^ Next byte to read
       -> State# RealWorld                  -- ^ World state before
       -> (# State# RealWorld, Addr#, a #)  
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
ioToDecoder (IO io) = Decoder $ \_ !(Ptr ip0) s0 -> case io s0 of
    (# s1, x #) -> (# s1, ip0, x #)

-- | A 'DecodeStep' that fails with the given message.
failStep :: String -> DecodeStep a
failStep msg ip0 s0 =
    case runIO (throw (DecodingException msg ip0)) s0 of
      -- unreachable, but makes the type checker happy.
      (# s1, x #) -> (# s1, getPtr ip0, x #)


-- Instances
------------

instance Functor Decoder where
    fmap f = \(Decoder io) -> Decoder $ \pd ip0 s0 -> 
        case io pd ip0 s0 of
            (# s1, ip1, x #) -> (# s1, ip1, f x #)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure x = Decoder $ \_ !(Ptr ip0) s0 -> (# s0, ip0, x #)

    {-# INLINE (<*>) #-}
    Decoder fIO <*> Decoder xIO = Decoder $ \pd ip0 s0 ->
        case fIO pd ip0 s0 of
          (# s1, ip1, f #) -> case xIO pd (Ptr ip1) s1 of
            (# s2, ip2, x #) -> (# s2, ip2, f x #)

instance Monad Decoder where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    Decoder xIO >>= f = Decoder $ \pd ip0 s0 ->
        case xIO pd ip0 s0 of
          (# s1, ip1, x #) -> unDecoder (f x) pd (Ptr ip1) s1

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

        (`catch` (handler ip0)) $ do
            x <- IO $ \s0 -> case unDecoder p pd ip0 s0 of
                               (# s1, _, x #) -> (# s1, x #)
            return (Right x)
  where
    handler :: Ptr Word8 -> DecodingException -> IO (Either String a)
    handler ip0 (DecodingException msg ip) = return $ Left $ 
        msg ++ 
        " (at byte " ++ show (ip `minusPtr` ip0) ++ " of " ++ show len ++ ")" 

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
word8 = Decoder $ \pd ip0 s0 -> case pdWord8 pd ip0 s0 of
                                  (# s1, ip1, w #) -> (# s1, ip1, W8# w #)

word8s = decodeList word8

{-# INLINABLE decodeList #-}
decodeList :: Decoder a -> Decoder [a]
decodeList x = go
  where
    go = do
      tag <- word8
      case tag of
        0 -> return []
        1 -> (:) <$> x <*> go
        _ -> fail $ "ParamDecoder.word8s: unexpected tag " ++ show tag

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
word = prim pdWord

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
int = prim pdInt

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
char = prim pdChar

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


