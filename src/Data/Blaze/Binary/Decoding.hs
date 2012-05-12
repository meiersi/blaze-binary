{-# LANGUAGE CPP, UnboxedTuples, MagicHash, ScopedTypeVariables, BangPatterns, DeriveDataTypeable, OverloadedStrings #-}
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
-- Binary encoding of values.
--
-----------------------------------------------------------------------------
module Data.Blaze.Binary.Decoding where

import Prelude hiding (catch)

import Control.Applicative
import Control.Exception

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Typeable
import qualified Data.ByteString.Internal as S
import GHC.Prim
import GHC.Ptr
import GHC.Word
import GHC.Exts
import GHC.IO (IO(IO))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Storable (Storable, sizeOf, peek)

#if  __GLASGOW_HASKELL__ >= 702
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#else
import Foreign.ForeignPtr (unsafeForeignPtrToPtr)
#endif

data ParseException = ParseException String -- {-# UNPACK #-} !(Ptr Word8)
  deriving( Show, Typeable )

instance Exception ParseException where

newtype Decoder a = Decoder {
          -- unDecoder :: ForeignPtr Word8 -> Addr# -> Addr#
          unDecoder :: ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8
                    -> State# RealWorld -> (# State# RealWorld, Addr#, a #)
        }

instance Functor Decoder where
    fmap f = \(Decoder io) -> Decoder $ \fpbuf ip0 ipe s0 ->
        case io fpbuf ip0 ipe s0 of
            (# s1, ip1, x #) -> (# s1, ip1, f x #)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure x = Decoder $ \_ ip0 _ s0 -> (# s0, getAddr ip0, x #)

    {-# INLINE (<*>) #-}
    Decoder fIO <*> Decoder xIO = Decoder $ \fpbuf ip0 ipe s0 ->
        case fIO fpbuf ip0 ipe s0 of
          (# s1, ip1, f #) -> case xIO fpbuf (Ptr ip1) ipe s1 of
            (# s2, ip2, x #) -> (# s2, ip2, f x #)

{-# INLINE liftIO #-}
liftIO :: IO a -> Decoder a
liftIO (IO io) = Decoder $ \_ !(Ptr ip0) _ s0 -> case io s0 of
  (# s1, x #) -> (# s1, ip0, x #)

{-# INLINE runIO #-}
runIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
runIO (IO io) = io

instance Monad Decoder where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    Decoder xIO >>= f = Decoder $ \fpbuf ip0 ipe s0 ->
        case xIO fpbuf ip0 ipe s0 of
          (# s1, ip1, x #) -> unDecoder (f x) fpbuf (Ptr ip1) ipe s1

    {-# INLINE fail #-}
    fail msg = liftIO $ throw $ ParseException msg


{-
requires :: Int -> Decoder a -> Decoder a
requires n p = Decoder $ \buf@(Buffer ip ipe) ->
    if ipe `minusPtr` ip >= n
      then unDecoder p buf
      else throw $ ParseException $
             "required " ++ show n ++
             " bytes, but there are only " ++ show (ipe `minusPtr` ip) ++
             " bytes left."
-}

{-# INLINE storable #-}
storable :: forall a. Storable a => Decoder a
storable = Decoder $ \fpbuf ip0 ipe s0 ->
    let ip1 = ip0 `plusPtr` size in
      if ip1 <= ipe
        then case runIO (peek (castPtr ip0 :: Ptr a)) s0 of
               (# s1, x #) -> (# s1, getAddr ip1, x #)
        else unDecoder
                (fail $ "less than the required " ++ show size ++ " bytes left.")
                fpbuf ip0 ipe s0
  where
    size = sizeOf (undefined :: a)

runDecoder :: Decoder a -> S.ByteString -> Either String a
runDecoder p (S.PS fpbuf off len) = S.inlinePerformIO $ do
    withForeignPtr fpbuf $ \pbuf -> do
        let !ip  = pbuf `plusPtr` off
            !ipe = ip `plusPtr` len
        (`catch` handler) $ do
            x <- IO $ \s0 -> case unDecoder p fpbuf ip ipe s0 of
                               (# s1, _, x #) -> (# s1, x #)
            return (Right x)
  where
    handler :: ParseException -> IO (Either String a)
    handler (ParseException msg) = return $ Left msg

-- Primitive parsers
--------------------

{-# INLINE word8 #-}
word8 :: Decoder Word8
word8 = storable

{-# INLINE word16 #-}
word16 :: Decoder Word16
word16 = storable

{-# INLINE word32 #-}
word32 :: Decoder Word32
word32 = storable

{-# INLINE word64 #-}
word64 :: Decoder Word64
word64 = storable

{-# INLINE word #-}
word :: Decoder Word
word = storable

{-# INLINE int8 #-}
int8 :: Decoder Int8
int8 = storable

{-# INLINE int16 #-}
int16 :: Decoder Int16
int16 = storable

{-# INLINE int32 #-}
int32 :: Decoder Int32
int32 = storable

{-# INLINE int64 #-}
int64 :: Decoder Int64
int64 = storable

{-# INLINE int #-}
int :: Decoder Int
int = storable

{-# INLINE float #-}
float :: Decoder Float
float = storable

{-# INLINE double #-}
double :: Decoder Double
double = storable

{-# INLINE byteString #-}
byteString :: Decoder S.ByteString
byteString = int >>= byteStringSlice

{-# INLINE byteStringSlice #-}
byteStringSlice :: Int -> Decoder S.ByteString
byteStringSlice len = Decoder $ \fpbuf ip0 ipe s0 ->
    let ip1 = ip0 `plusPtr` len
    in
      if ip1 <= ipe
        then (# s0
             , getAddr ip1
             ,  S.PS fpbuf (ip0 `minusPtr` unsafeForeignPtrToPtr fpbuf) len
             #)
        else unDecoder
                (fail $ "less than the required " ++ show len ++ " bytes left.")
                fpbuf ip0 ipe s0

char :: Decoder Char
char = do
    w0 <- word8
    case () of
      _ | w0 < 0x80 -> return (chr1 w0)
        | w0 < 0xe0 -> chr2 w0 <$> word8
        | w0 < 0xf0 -> chr3 w0 <$> word8 <*> word8
        | otherwise -> chr4 w0 <$> word8 <*> word8 <*> word8

{-# INLINE getAddr #-}
getAddr :: Ptr a -> Addr#
getAddr (Ptr a) = a

-- Decoder combinators
--------------------

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


word8sSimple :: Decoder [Word8]
word8sSimple = decodeList word8

word8s :: Decoder [Word8]
word8s =
    go []
  where
    go xs = do
        tag <- word8
        case tag of
          0 -> return (reverse xs)
          1 -> do x <- word8
                  go (x:xs)
          _ -> fail $ "word8s: unexpected tag " ++ show tag

------------------------------------------------------------------------------
-- UTF-8 decoding helpers
------------------------------------------------------------------------------

chr1 :: Word8 -> Char
chr1 (W8# x#) = C# (chr# (word2Int# x#))
{-# INLINE chr1 #-}

chr2 :: Word8 -> Word8 -> Char
chr2 (W8# x1#) (W8# x2#) = C# (chr# (z1# +# z2#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
      !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3 :: Word8 -> Word8 -> Word8 -> Char
chr3 (W8# x1#) (W8# x2#) (W8# x3#) = C# (chr# (z1# +# z2# +# z3#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
      !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4             :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    C# (chr# (z1# +# z2# +# z3# +# z4#))
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
