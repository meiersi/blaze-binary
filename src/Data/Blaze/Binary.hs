{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
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

    ) where

import Data.Blaze.Binary.Encoding

import Data.Word
import Data.Monoid
import Data.Foldable (foldMap)
import Foreign

-- And needed for the instances:
import           Data.Array.Unboxed
import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L (foldrChunks)
import qualified Data.ByteString.Lazy.Builder  as B
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.IntMap                   as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Ratio                    as R
import qualified Data.Tree                     as T
import qualified Data.Sequence                 as Seq


------------------------------------------------------------------------

type Encoding t = t -> VStream

-- | If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (@ghc >= 7.2.1@), the 'encode' and 'get'
-- methods will have default generic implementations.
--
-- To use this option, simply add a @deriving 'Generic'@ clause to your datatype
-- and declare a 'Binary' instance for it without giving a definition for
-- 'encode' and 'get'.
class Binary t where
    -- | Encode a value in the Put monad.
    encode :: Encoding t

-- | Encode a value to a strict 'S.ByteString'.
toByteString :: Binary t => t -> S.ByteString
-- FIXME: Use more efficient conversion.
toByteString = S.concat . L.toChunks . toLazyByteString


-- | Encode a value to a lazy 'L.ByteString'.
toLazyByteString :: Binary t => t -> L.ByteString
toLazyByteString = B.toLazyByteString . render . encode

------------------------------------------------------------------------
-- Simple instances

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Binary () where
    {-# INLINE encode #-}
    encode ()  = mempty

-- Bools are encoded as a byte in the range 0 .. 1
instance Binary Bool where
    {-# INLINE encode #-}
    encode     = word8 . fromIntegral . fromEnum

-- Values of type 'Ordering' are encoded as a byte in the range 0 .. 2
instance Binary Ordering where
    {-# INLINE encode #-}
    encode     = word8 . fromIntegral . fromEnum

------------------------------------------------------------------------
-- Words and Ints

-- Words8s are written as bytes
instance Binary Word8 where
    {-# INLINE encode #-}
    encode     = word8

-- Words16s are written as 2 bytes in big-endian (network) order
instance Binary Word16 where
    {-# INLINE encode #-}
    encode     = word16

-- Words32s are written as 4 bytes in big-endian (network) order
instance Binary Word32 where
    {-# INLINE encode #-}
    encode     = word32

-- Words64s are written as 8 bytes in big-endian (network) order
instance Binary Word64 where
    {-# INLINE encode #-}
    encode     = word64

-- Int8s are written as a single byte.
instance Binary Int8 where
    {-# INLINE encode #-}
    encode     = int8

-- Int16s are written as a 2 bytes in big endian format
instance Binary Int16 where
    {-# INLINE encode #-}
    encode     = int16

-- Int32s are written as a 4 bytes in big endian format
instance Binary Int32 where
    {-# INLINE encode #-}
    encode     = int32

-- Int64s are written as a 8 bytes in big endian format
instance Binary Int64 where
    {-# INLINE encode #-}
    encode     = int64

------------------------------------------------------------------------

-- Words are are written as Word64s, that is, 8 bytes in big endian format
instance Binary Word where
    {-# INLINE encode #-}
    encode   = word

-- Ints are are written as Int64s, that is, 8 bytes in big endian format
instance Binary Int where
    {-# INLINE encode #-}
    encode  = int

instance Binary Integer where
    {-# INLINE encode #-}
    encode = integer

instance (Binary a, Integral a) => Binary (R.Ratio a) where
    {-# INLINE encode #-}
    encode = \r -> encode (R.numerator r) <> encode (R.denominator r)

instance Binary Char where
    {-# INLINE encode #-}
    encode = char

instance (Binary a, Binary b) => Binary (a,b) where
    {-# INLINE encode #-}
    encode (a,b) = encode a <> encode b

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    {-# INLINE encode #-}
    encode (a,b,c) = encode a <> encode b <> encode c

instance (Binary a, Binary b, Binary c, Binary d)
        => Binary (a,b,c,d) where
    encode (a,b,c,d) = encode a <> encode b <> encode c <> encode d

instance (Binary a, Binary b, Binary c, Binary d, Binary e)
        => Binary (a,b,c,d,e) where
    encode (a,b,c,d,e) = encode a <> encode b <> encode c <> encode d <> encode e

-- 
-- and now just recurse:
--

instance (Binary a, Binary b, Binary c, Binary d, Binary e
         , Binary f)
        => Binary (a,b,c,d,e,f) where
    encode (a,b,c,d,e,f) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f
 

instance (Binary a, Binary b, Binary c, Binary d, Binary e
         , Binary f, Binary g)
        => Binary (a,b,c,d,e,f,g) where
    encode (a,b,c,d,e,f,g) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h)
        => Binary (a,b,c,d,e,f,g,h) where
    encode (a,b,c,d,e,f,g,h) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g <> encode h

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h, Binary i)
        => Binary (a,b,c,d,e,f,g,h,i) where
    encode (a,b,c,d,e,f,g,h,i) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g <> encode h <> encode i

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h, Binary i, Binary j)
        => Binary (a,b,c,d,e,f,g,h,i,j) where
    encode (a,b,c,d,e,f,g,h,i,j) = encode a <> encode b <> encode c <> encode d <> encode e <> encode f <> encode g <> encode h <> encode i <> encode j

------------------------------------------------------------------------
-- Container types

-- | Share list encoding, as it is required for faster tree encoding.
{-# INLINE encodeList #-}
encodeList :: Encoding a -> Encoding [a]
encodeList f = (<> word8 0) . foldMap ((word8 1 <>) . f)
-- Encoding the list in reverse order might be interesting to simplify its
-- parsing. It just depends on which side is easier to get up to speed :-)
-- encodeList f = (<> word8 0) . foldl (\lhs x -> word8 1 <> f x <> lhs) mempty

instance Binary a => Binary [a] where
    {-# INLINE encode #-}
    encode = encodeList encode

instance (Binary a) => Binary (Maybe a) where
    {-# INLINE encode #-}
    encode = maybe (word8 0) ((word8 1 <>) . encode)

instance (Binary a, Binary b) => Binary (Either a b) where
    {-# INLINE encode #-}
    encode = either ((word8 0 <>) . encode) ((word8 1 <>) . encode)

------------------------------------------------------------------------
-- ByteStrings (have specially efficient instances)

instance Binary S.ByteString where
    {-# INLINE encode #-}
    encode = \bs -> int (S.length bs) <> byteString bs

instance Binary L.ByteString where
    encode = (<> int 0) . L.foldrChunks (\bs s -> encode bs <> s) mempty

------------------------------------------------------------------------
-- Maps and Sets

instance (Ord a, Binary a) => Binary (Set.Set a) where
    {-# INLINE encode #-}
    encode = encode . Set.toAscList

instance (Ord k, Binary k, Binary e) => Binary (Map.Map k e) where
    {-# INLINE encode #-}
    encode = encode . Map.toAscList

instance Binary IntSet.IntSet where
    {-# INLINE encode #-}
    encode = encode . IntSet.toAscList

instance (Binary e) => Binary (IntMap.IntMap e) where
    {-# INLINE encode #-}
    encode = encode . IntMap.toAscList

------------------------------------------------------------------------
-- Queues and Sequences

instance (Binary e) => Binary (Seq.Seq e) where
    {-# INLINE encode #-}
    encode = \s -> int (Seq.length s) <> foldMap encode s

------------------------------------------------------------------------
-- Floating point

instance Binary Double where
    {-# INLINE encode #-}
    encode = double

instance Binary Float where
    {-# INLINE encode #-}
    encode = float

------------------------------------------------------------------------
-- Trees

instance (Binary e) => Binary (T.Tree e) where
    {-# INLINE encode #-}
    encode =
        go
      where 
        go (T.Node x cs) = encode x <> encodeList go cs

------------------------------------------------------------------------
-- Arrays

instance (Binary i, Ix i, Binary e) => Binary (Array i e) where
    {-# INLINE encode #-}
    encode = \a -> encode (bounds a) <> encode (elems a)
--
-- The IArray UArray e constraint is non portable. Requires flexible instances
--
instance (Binary i, Ix i, Binary e, IArray UArray e) => Binary (UArray i e) where
    {-# INLINE encode #-}
    encode = \a -> encode (bounds a) <> encode (elems a)

