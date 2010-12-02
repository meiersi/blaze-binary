{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Put
-- Copyright   : 2010 Simon Meier
--               API based on orignal implementation by Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : stable
-- Portability : Tested on GHC only.
--
-- The Put monad. A monad for efficiently constructing lazy bytestrings.
--
-----------------------------------------------------------------------------

module Data.Binary.Put (

    -- * The Put type
      Put
    , PutM(..)
    , runPut
    , runPutM
    , putBuilder
    , execPut

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

    -- * Host-endian, unaligned writes
    , putWordhost           -- :: Word   -> Put
    , putWord16host         -- :: Word16 -> Put
    , putWord32host         -- :: Word32 -> Put
    , putWord64host         -- :: Word64 -> Put

  ) where

import qualified Data.Binary.Builder          as B hiding (toLazyByteString)
import qualified Data.Binary.Builder.Internal as B

import Data.Word
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

#ifdef APPLICATIVE_IN_BASE
import Control.Applicative
#endif


------------------------------------------------------------------------

-- | The PutM type. A Writer monad over the efficient Builder monoid
-- implemented using continuation passing.
type PutM a = B.Put a

-- | Put merely lifts Builder into a Writer monad, applied to ().
type Put = PutM ()

putBuilder ::B.Builder -> Put
putBuilder = B.putBuilder
{-# INLINE putBuilder #-}

-- | Run the 'Put' monad
execPut :: PutM a ->B.Builder
execPut = B.fromPut
{-# INLINE execPut #-}

-- | Run the 'Put' monad with a serialiser
runPut :: Put -> L.ByteString
runPut = snd . runPutM
{-# INLINE runPut #-}

-- | Run the 'Put' monad with a serialiser and get its result. Note that
-- forcing the result also forces all chunks of the lazy bytestring.
runPutM :: PutM a -> (a, L.ByteString)
runPutM = B.toLazyByteString
{-# INLINE runPutM #-}

------------------------------------------------------------------------

-- | Pop the ByteString we have constructed so far, if any, yielding a
-- new chunk in the result ByteString.
flush               :: Put
flush               = putBuilder B.flush
{-# INLINE flush #-}

-- | Efficiently write a byte into the output buffer
putWord8            :: Word8 -> Put
putWord8            = putBuilder . B.fromWord8
{-# INLINE putWord8 #-}

-- | An efficient primitive to write a strict ByteString into the output buffer.
-- It flushes the current buffer, and writes the argument into a new chunk.
putByteString       :: S.ByteString -> Put
putByteString       = putBuilder . B.fromByteString
{-# INLINE putByteString #-}

-- | Write a lazy ByteString efficiently, simply appending the lazy
-- ByteString chunks to the output buffer
putLazyByteString   :: L.ByteString -> Put
putLazyByteString   = putBuilder . B.fromLazyByteString
{-# INLINE putLazyByteString #-}

-- | Write a Word16 in big endian format
putWord16be         :: Word16 -> Put
putWord16be         = putBuilder . B.fromWord16be
{-# INLINE putWord16be #-}

-- | Write a Word16 in little endian format
putWord16le         :: Word16 -> Put
putWord16le         = putBuilder . B.fromWord16le
{-# INLINE putWord16le #-}

-- | Write a Word32 in big endian format
putWord32be         :: Word32 -> Put
putWord32be         = putBuilder . B.fromWord32be
{-# INLINE putWord32be #-}

-- | Write a Word32 in little endian format
putWord32le         :: Word32 -> Put
putWord32le         = putBuilder . B.fromWord32le
{-# INLINE putWord32le #-}

-- | Write a Word64 in big endian format
putWord64be         :: Word64 -> Put
putWord64be         = putBuilder . B.fromWord64be
{-# INLINE putWord64be #-}

-- | Write a Word64 in little endian format
putWord64le         :: Word64 -> Put
putWord64le         = putBuilder . B.fromWord64le
{-# INLINE putWord64le #-}

------------------------------------------------------------------------

-- | /O(1)./ Write a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putWordhost         :: Word -> Put
putWordhost         = putBuilder . B.fromWordhost
{-# INLINE putWordhost #-}

-- | /O(1)./ Write a Word16 in native host order and host endianness.
-- For portability issues see @putWordhost@.
putWord16host       :: Word16 -> Put
putWord16host       = putBuilder . B.fromWord16host
{-# INLINE putWord16host #-}

-- | /O(1)./ Write a Word32 in native host order and host endianness.
-- For portability issues see @putWordhost@.
putWord32host       :: Word32 -> Put
putWord32host       = putBuilder . B.fromWord32host
{-# INLINE putWord32host #-}

-- | /O(1)./ Write a Word64 in native host order
-- On a 32 bit machine we write two host order Word32s, in big endian form.
-- For portability issues see @putWordhost@.
putWord64host       :: Word64 -> Put
putWord64host       = putBuilder . B.fromWord64host
{-# INLINE putWord64host #-}
