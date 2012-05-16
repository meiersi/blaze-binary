{-# LANGUAGE CPP, BangPatterns, FlexibleContexts, FlexibleInstances #-}

#ifdef GENERICS
{-# LANGUAGE DefaultSignatures
           , TypeOperators
           , BangPatterns
           , KindSignatures
           , ScopedTypeVariables
  #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Blaze.Binary
-- Copyright   : 2012, Simon Meier <iridcode@gmail.com>
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   :
-- Portability :
--
-----------------------------------------------------------------------------

module Data.Blaze.Binary (

    -- * The Binary class
      Binary(..)
    , toByteString
    , toLazyByteString

    , fromByteString
    , fromLazyByteString

    ) where

import Control.Applicative

import qualified Data.Blaze.Binary.Encoder as E
import qualified Data.Blaze.Binary.Decoder as D

import Data.Word
import Data.Monoid
import Data.Foldable (foldMap)
import Foreign

-- And needed for the instances:
import           Data.Array.Unboxed
import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L (foldrChunks, ByteString(..))
import qualified Data.ByteString.Lazy.Builder  as B
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.IntMap                   as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Ratio                    as R
import qualified Data.Tree                     as T
import qualified Data.Sequence                 as Seq

#ifdef GENERICS
import GHC.Generics
#endif

------------------------------------------------------------------------

-- | If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (@ghc >= 7.2.1@), the 'encode' and 'decode'
-- methods will have default generic implementations.
--
-- To use this option, simply add a @deriving 'Generic'@ clause to your datatype
-- and declare a 'Binary' instance for it without giving a definition for
-- 'encode' and 'decode'.
class Binary t where
    -- | Encode a value in the Put monad.
    encode :: E.Encoder t
    decode :: D.Decoder t

#ifdef GENERICS
    default encode :: (Generic t, GBinary (Rep t)) => E.Encoder t
    encode = gEncode . from
    {-# INLINE encode #-}

    default decode :: (Generic t, GBinary (Rep t)) => D.Decoder t
    decode = to <$> gDecode
    {-# INLINE decode #-}
#endif

-- | Encode a value to a strict 'S.ByteString'.
toByteString :: Binary t => t -> S.ByteString
-- FIXME: Use more efficient conversion.
toByteString = S.concat . L.toChunks . toLazyByteString


-- | Encode a value to a lazy 'L.ByteString'.
toLazyByteString :: Binary t => t -> L.ByteString
toLazyByteString = B.toLazyByteString . E.render . encode

-- | Decode a value from a strict 'S.ByteString'.
-- TODO: Return unparsed input.
fromByteString :: Binary t => S.ByteString -> Either String t
fromByteString = D.runDecoder decode

-- | Decode a value from a lazy 'L.ByteString'.
-- TODO: Return unparsed input.
fromLazyByteString :: Binary t => L.ByteString -> Either String t
fromLazyByteString = fromByteString . S.concat . L.toChunks


------------------------------------------------------------------------
-- Simple instances

wrongTag :: Show a => String -> a -> D.Decoder b
wrongTag loc tag = fail $ "decode " ++ loc ++ ": could not parse tag " ++ show tag

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Binary () where
    {-# INLINE encode #-}
    encode () = mempty
    {-# INLINE decode #-}
    decode = return ()

-- Bools are encoded as a byte in the range 0 .. 1
instance Binary Bool where
    {-# INLINE encode #-}
    encode = \x -> E.word8 (if x then 1 else 0)
    decode = do tag <- D.word8
                case tag of
                  0 -> return False
                  1 -> return True
                  _ -> wrongTag "Bool" tag

-- Values of type 'Ordering' are encoded as a byte in the range 0 .. 2
instance Binary Ordering where
    {-# INLINE encode #-}
    encode = \x -> E.word8 (case x of LT -> 0; EQ -> 1; GT -> 2)
    decode = do tag <- D.word8
                case tag of
                  0 -> return LT
                  1 -> return EQ
                  2 -> return GT
                  _ -> wrongTag "Ordering" tag

------------------------------------------------------------------------
-- Words and Ints

-- Words8s are written as bytes
instance Binary Word8 where
    {-# INLINE encode #-}
    encode = E.word8
    {-# INLINE decode #-}
    decode = D.word8

-- Words16s are written as 2 bytes in big-endian (network) order
instance Binary Word16 where
    {-# INLINE encode #-}
    encode  = E.word16
    {-# INLINE decode #-}
    decode = D.word16

-- Words32s are written as 4 bytes in big-endian (network) order
instance Binary Word32 where
    {-# INLINE encode #-}
    encode = E.word32
    {-# INLINE decode #-}
    decode = D.word32

-- Words64s are written as 8 bytes in big-endian (network) order
instance Binary Word64 where
    {-# INLINE encode #-}
    encode = E.word64
    {-# INLINE decode #-}
    decode = D.word64

-- Int8s are written as a single byte.
instance Binary Int8 where
    {-# INLINE encode #-}
    encode = E.int8
    {-# INLINE decode #-}
    decode = D.int8

-- Int16s are written as a 2 bytes in big endian format
instance Binary Int16 where
    {-# INLINE encode #-}
    encode = E.int16
    {-# INLINE decode #-}
    decode = D.int16

-- Int32s are written as a 4 bytes in big endian format
instance Binary Int32 where
    {-# INLINE encode #-}
    encode = E.int32
    {-# INLINE decode #-}
    decode = D.int32

-- Int64s are written as a 8 bytes in big endian format
instance Binary Int64 where
    {-# INLINE encode #-}
    encode = E.int64
    {-# INLINE decode #-}
    decode = D.int64

------------------------------------------------------------------------

-- Words are are written as Word64s, that is, 8 bytes in big endian format
instance Binary Word where
    {-# INLINE encode #-}
    encode = E.word
    {-# INLINE decode #-}
    decode = D.word

-- Ints are are written as Int64s, that is, 8 bytes in big endian format
instance Binary Int where
    {-# INLINE encode #-}
    encode = E.int
    {-# INLINE decode #-}
    decode = D.int

instance Binary Integer where
    {-# INLINE encode #-}
    encode = E.integer
    decode = D.integer

instance (Binary a, Integral a) => Binary (R.Ratio a) where
    {-# INLINE encode #-}
    encode = \r -> encode (R.numerator r) <> encode (R.denominator r)
    {-# INLINE decode #-}
    decode = (R.%) <$> decode <*> decode

instance Binary Char where
    {-# INLINE encode #-}
    encode = E.char
    {-# INLINE decode #-}
    decode = D.char

instance (Binary a, Binary b) => Binary (a,b) where
    {-# INLINE encode #-}
    encode (a,b) = encode a <> encode b
    {-# INLINE decode #-}
    decode = (,) <$> decode <*> decode

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    {-# INLINE encode #-}
    encode (a,b,c) = encode a <> encode b <> encode c
    {-# INLINE decode #-}
    decode = (,,) <$> decode <*> decode <*> decode

instance (Binary a, Binary b, Binary c, Binary d)
        => Binary (a,b,c,d) where
    encode (a,b,c,d) = encode a <> encode b <> encode c <> encode d
    decode = (,,,) <$> decode <*> decode <*> decode <*> decode

instance (Binary a, Binary b, Binary c, Binary d, Binary e)
        => Binary (a,b,c,d,e) where
    encode (a,b,c,d,e) = encode a <> encode b <> encode c <> encode d <> encode e
    decode = (,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode

--
-- and now just recurse:
--

instance (Binary a, Binary b, Binary c, Binary d, Binary e
         , Binary f)
        => Binary (a,b,c,d,e,f) where
    encode (a,b,c,d,e,f) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f
    decode = (,,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode


instance (Binary a, Binary b, Binary c, Binary d, Binary e
         , Binary f, Binary g)
        => Binary (a,b,c,d,e,f,g) where
    encode (a,b,c,d,e,f,g) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g
    decode = (,,,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h)
        => Binary (a,b,c,d,e,f,g,h) where
    encode (a,b,c,d,e,f,g,h) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g <> encode h
    decode = (,,,,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h, Binary i)
        => Binary (a,b,c,d,e,f,g,h,i) where
    encode (a,b,c,d,e,f,g,h,i) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g <> encode h <> encode i
    decode = (,,,,,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h, Binary i, Binary j)
        => Binary (a,b,c,d,e,f,g,h,i,j) where
    encode (a,b,c,d,e,f,g,h,i,j) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g <> encode h <> encode i <> encode j
    decode = (,,,,,,,,,) <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode

------------------------------------------------------------------------
-- Container types

instance Binary a => Binary [a] where
    {-# INLINE encode #-}
    encode = E.encodeList encode
    {-# INLINE decode #-}
    decode = D.decodeList decode

instance (Binary a) => Binary (Maybe a) where
    {-# INLINE encode #-}
    encode = maybe (E.word8 0) ((E.word8 1 <>) . encode)
    {-# INLINE decode #-}
    decode = D.decodeMaybe decode

instance (Binary a, Binary b) => Binary (Either a b) where
    {-# INLINE encode #-}
    encode = either ((E.word8 0 <>) . encode) ((E.word8 1 <>) . encode)
    {-# INLINE decode #-}
    decode = D.decodeEither decode decode

------------------------------------------------------------------------
-- ByteStrings (have specially efficient instances)

instance Binary S.ByteString where
    {-# INLINE encode #-}
    encode = E.byteString
    {-# INLINE decode #-}
    decode = D.byteString

instance Binary L.ByteString where
    encode = L.foldrChunks (\bs s -> encode bs <> s) (encode S.empty)
    decode = do
      bs <- decode
      if S.null bs
        then return L.Empty
        else L.Chunk bs <$> decode

------------------------------------------------------------------------
-- Maps and Sets

instance (Ord a, Binary a) => Binary (Set.Set a) where
    {-# INLINE encode #-}
    encode = encode . Set.toAscList
    {-# INLINE decode #-}
    decode = Set.fromAscList <$> decode

instance (Ord k, Binary k, Binary e) => Binary (Map.Map k e) where
    {-# INLINE encode #-}
    encode = encode . Map.toAscList
    {-# INLINE decode #-}
    decode = Map.fromAscList <$> decode

instance Binary IntSet.IntSet where
    {-# INLINE encode #-}
    encode = encode . IntSet.toAscList
    {-# INLINE decode #-}
    decode = IntSet.fromAscList <$> decode

instance (Binary e) => Binary (IntMap.IntMap e) where
    {-# INLINE encode #-}
    encode = encode . IntMap.toAscList
    {-# INLINE decode #-}
    decode = IntMap.fromAscList <$> decode

------------------------------------------------------------------------
-- Queues and Sequences

instance (Binary e) => Binary (Seq.Seq e) where
    {-# INLINE encode #-}
    encode = \s -> E.int (Seq.length s) <> foldMap encode s
    {-# INLINE decode #-}
    decode = do
        D.int >>= go Seq.empty
      where
        go !s !len
          | len <= 0  = return s
          | otherwise = do
              x <- decode
              go (s Seq.|> x) (len - 1)


------------------------------------------------------------------------
-- Floating point

instance Binary Double where
    {-# INLINE encode #-}
    encode = E.double
    {-# INLINE decode #-}
    decode = D.double

instance Binary Float where
    {-# INLINE encode #-}
    encode = E.float
    {-# INLINE decode #-}
    decode = D.float

------------------------------------------------------------------------
-- Trees

instance (Binary e) => Binary (T.Tree e) where
    {-# INLINE encode #-}
    encode =
        go
      where
        go (T.Node x cs) = encode x <> E.encodeList go cs

    {-# INLINE decode #-}
    decode =
        go
      where
        go = T.Node <$> decode <*> D.decodeList go


------------------------------------------------------------------------
-- Arrays

instance (Binary i, Ix i, Binary e) => Binary (Array i e) where
    {-# INLINE encode #-}
    encode = \a -> encode (bounds a) <> encode (elems a)
    {-# INLINE decode #-}
    decode = listArray <$> decode <*> decode

--
-- The IArray UArray e constraint is non portable. Requires flexible instances
--
instance (Binary i, Ix i, Binary e, IArray UArray e) => Binary (UArray i e) where
    {-# INLINE encode #-}
    encode = \a -> encode (bounds a) <> encode (elems a)
    {-# INLINE decode #-}
    decode = listArray <$> decode <*> decode

#ifdef GENERICS
------------------------------------------------------------------------
-- Generic Binary

class GBinary f where
    gEncode :: E.Encoder  (f a)
    gDecode :: D.Decoder (f a)

instance GBinary a => GBinary (M1 i c a) where
    gEncode = gEncode . unM1
    gDecode = M1 <$> gDecode
    {-# INLINE gEncode #-}
    {-# INLINE gDecode #-}

instance Binary a => GBinary (K1 i a) where
    gEncode = encode . unK1
    gDecode = K1 <$> decode
    {-# INLINE gEncode #-}
    {-# INLINE gDecode #-}

instance GBinary U1 where
    gEncode = const mempty
    gDecode = pure U1
    {-# INLINE gEncode #-}
    {-# INLINE gDecode #-}

instance (GBinary a, GBinary b) => GBinary (a :*: b) where
    gEncode (a :*: b) = gEncode a <> gEncode b
    gDecode = (:*:) <$> gDecode  <*> gDecode
    {-# INLINE gEncode #-}
    {-# INLINE gDecode #-}

-- The following GBinary instance for sums has support for serializing types
-- with up to 2^64-1 constructors. It will use the minimal number of bytes
-- needed to encode the constructor. For example when a type has 2^8
-- constructors or less it will use a single byte to encode the constructor. If
-- it has 2^16 constructors or less it will use two bytes, and so on till 2^64-1.

#define GUARD(WORD) (size - 1) <= fromIntegral (maxBound :: WORD)
#define ENCODESUM(WORD) GUARD(WORD) = encodeSum (0 :: WORD) (fromIntegral size)
#define DECODESUM(WORD) GUARD(WORD) = (decode :: D.Decoder WORD) >>= checkDecodeSum (fromIntegral size)

instance ( EncodeSum a, EncodeSum b
         , DecodeSum a, DecodeSum b
         , GBinary   a, GBinary   b
         , SumSize   a, SumSize   b) => GBinary (a :+: b) where
    gEncode | ENCODESUM(Word8) | ENCODESUM(Word16) | ENCODESUM(Word32) | ENCODESUM(Word64)
            | otherwise = sizeError "encode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)

    gDecode | DECODESUM(Word8) | DECODESUM(Word16) | DECODESUM(Word32) | DECODESUM(Word64)
            | otherwise = sizeError "decode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)
    {-# INLINE gEncode #-}
    {-# INLINE gDecode #-}

sizeError :: Show size => String -> size -> error
sizeError s size = error $ "Can't " ++ s ++ " a type with " ++ show size ++ " constructors"

------------------------------------------------------------------------

class EncodeSum f where
    encodeSum :: (Num word, Bits word, Binary word) => word -> word -> E.Encoder (f a)

instance (EncodeSum a, EncodeSum b, GBinary a, GBinary b) => EncodeSum (a :+: b) where
    encodeSum !tag !size s = case s of
                               L1 x -> encodeSum tag           sizeL x
                               R1 x -> encodeSum (tag + sizeL) sizeR x
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL
    {-# INLINE encodeSum #-}

instance GBinary a => EncodeSum (C1 c a) where
    encodeSum !tag _ x = encode tag <> gEncode x
    {-# INLINE encodeSum #-}

------------------------------------------------------------------------

checkDecodeSum :: (Ord word, Bits word, DecodeSum f) => word -> word -> D.Decoder (f a)
checkDecodeSum size tag | tag < size = decodeSum tag size
                        | otherwise  = fail "Unknown encoding for constructor"
{-# INLINE checkDecodeSum #-}

class DecodeSum f where
    decodeSum :: (Ord word, Num word, Bits word) => word -> word -> D.Decoder (f a)

instance (DecodeSum a, DecodeSum b, GBinary a, GBinary b) => DecodeSum (a :+: b) where
    decodeSum !tag !size | tag < sizeL = L1 <$> decodeSum tag           sizeL
                         | otherwise   = R1 <$> decodeSum (tag - sizeL) sizeR
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL
    {-# INLINE decodeSum #-}

instance GBinary a => DecodeSum (C1 c a) where
    decodeSum _ _ = gDecode
    {-# INLINE decodeSum #-}

------------------------------------------------------------------------

class SumSize f where
    sumSize :: Tagged f Word64

newtype Tagged (s :: * -> *) b = Tagged {unTagged :: b}

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a Word64) +
                       unTagged (sumSize :: Tagged b Word64)
    {-# INLINE sumSize #-}

instance SumSize (C1 c a) where
    sumSize = Tagged 1
    {-# INLINE sumSize #-}
#endif
