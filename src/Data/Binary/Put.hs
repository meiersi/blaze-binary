-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Put
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Lennart Kolmodin <kolmodin@dtek.chalmers.se>
-- Stability   : stable
-- Portability : Portable to Hugs and GHC. Requires MPTCs
--
-- The Put monad. A monad for efficiently constructing lazy bytestrings.
--
-----------------------------------------------------------------------------

module Data.Binary.Put (

    -- * The Put type
      Put
    , runPut

    -- * Flushing the implicit parse state
    , flush

    -- * Primitives
    , putWord8
    , putByteString
    , putLazyByteString

    -- * Big-endian primitives
    , putWord16be
    , putWord32be
    , putWord64be

    -- * Little-endian primitives
    , putWord16le
    , putWord32le
    , putWord64le

  ) where

import Data.Binary.Builder (Builder, toLazyByteString)
import qualified Data.Binary.Builder as B

import Data.Word
import qualified Data.ByteString.Base as S
import qualified Data.ByteString.Lazy as L

------------------------------------------------------------------------

-- | The Put types. A Writer monad over the efficient Builder monoid.
-- Put merely lifts Builder into a monad
newtype Put_ a = Put { unPut :: (a, Builder) }
type Put = Put_ ()

instance Functor Put_ where
        fmap f m = Put (let (a, w) = unPut m
                         in (f a, w))

instance Monad Put_ where
        return a = Put (a, B.empty)

        m >>= k  = Put (let (a, w)  = unPut m
                            (b, w') = unPut (k a)
                         in (b, w `B.append` w'))

        m1 >> m2 = Put (let (_, w)  = unPut m1
                            (b, w') = unPut m2
                         in (b, w `B.append` w'))
        {-# INlINE (>>) #-}

tell :: Builder -> Put
tell b = Put ((), b)
{-# INlINE tell #-}

-- | Run the 'Put' monad with a serialiser
runPut              :: Put -> L.ByteString
runPut              = toLazyByteString . snd . unPut
{-# INLINE runPut #-}

-- | Pop the ByteString we have constructed so far, if any, yielding a
-- new chunk in the result ByteString.
flush               :: Put
flush               = tell B.flush

-- | Efficiently write a byte into the output buffer
putWord8            :: Word8 -> Put
putWord8            = tell . B.singleton
{-# INLINE putWord8 #-}

-- | An efficient primitive to write a strict ByteString into the output buffer.
-- It flushes the current buffer, and writes the argument into a new chunk.
putByteString       :: S.ByteString -> Put
putByteString       = tell . B.fromByteString

-- | Write a lazy ByteString efficiently, simply appending the lazy
-- ByteString chunks to the output buffer
putLazyByteString   :: L.ByteString -> Put
putLazyByteString   = tell . B.fromLazyByteString

-- | Write a Word16 in big endian format
putWord16be         :: Word16 -> Put
putWord16be         = tell . B.putWord16be

-- | Write a Word16 in little endian format
putWord16le         :: Word16 -> Put
putWord16le         = tell . B.putWord16le

-- | Write a Word32 in big endian format
putWord32be         :: Word32 -> Put
putWord32be         = tell . B.putWord32be

-- | Write a Word32 in little endian format
putWord32le         :: Word32 -> Put
putWord32le         = tell . B.putWord32le

-- | Write a Word64 in big endian format
putWord64be         :: Word64 -> Put
putWord64be         = tell . B.putWord64be

-- | Write a Word64 in little endian format
putWord64le         :: Word64 -> Put
putWord64le         = tell . B.putWord64le
