{-# LANGUAGE CPP, BangPatterns, Rank2Types #-}
-- |
-- Module      : Data.Binary.Builder.Internal.Buffer
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Execution of the 'Put' monad and hence also 'Builder's with respect to
-- buffers.
--
module Data.Binary.Builder.Internal.Buffer where

import Foreign
import qualified Data.ByteString      as S

#ifdef BYTESTRING_IN_BASE
import qualified Data.ByteString.Base as S
#else
import qualified Data.ByteString.Internal as S
#endif

import Data.Binary.Builder.Internal.Types

------------------------------------------------------------------------------
-- Executing puts and builders
------------------------------------------------------------------------------


data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8) -- underlying pinned array
                     {-# UNPACK #-} !(Ptr Word8)        -- beginning of slice
                     {-# UNPACK #-} !(Ptr Word8)        -- next free byte
                     {-# UNPACK #-} !(Ptr Word8)        -- first byte after buffer

allocBuffer :: Int -> IO Buffer
allocBuffer size = do
    fpbuf <- S.mallocByteString size
    let !pbuf = unsafeForeignPtrToPtr fpbuf
    return $! Buffer fpbuf pbuf pbuf (pbuf `plusPtr` size)

unsafeFreezeBuffer :: Buffer -> S.ByteString
unsafeFreezeBuffer (Buffer fpbuf p0 op _) = 
    S.PS fpbuf 0 (op `minusPtr` p0)

unsafeFreezeNonEmptyBuffer :: Buffer -> Maybe S.ByteString
unsafeFreezeNonEmptyBuffer (Buffer fpbuf p0 op _) 
  | p0 == op  = Nothing
  | otherwise = Just $ S.PS fpbuf 0 (op `minusPtr` p0)

nextSlice :: Int -> Buffer -> Maybe Buffer
nextSlice minSize (Buffer fpbuf _ op ope)
  | ope `minusPtr` op <= minSize = Nothing
  | otherwise                    = Just (Buffer fpbuf op op ope)

runPut :: Monad m 
       => (IO (BuildSignal a) -> m (BuildSignal a)) -- lifting of buildsteps
       -> (Int -> Buffer -> m Buffer) -- output function for a guaranteedly non-empty buffer, the returned buffer will be filled next
       -> (S.ByteString -> m ())    -- output function for guaranteedly non-empty bytestrings, that are inserted directly into the stream
       -> Put a                     -- put to execute
       -> Buffer                    -- initial buffer to be used
       -> m (a, Buffer)             -- result of put and remaining buffer
runPut liftIO outputBuf outputBS (Put put) =
    runStep (put (finalStep))
  where
    finalStep x = buildStep $ \(BufRange op _) -> return $ Done op x

    runStep step buf@(Buffer fpbuf p0 op ope) = do
        let !br = BufRange op ope
        signal <- liftIO $ runBuildStep step br
        case signal of 
            Done op' x ->         -- put completed, buffer partially runSteped
                return (x, Buffer fpbuf p0 op' ope)

            BufferFull minSize op' nextStep -> do
                buf' <- outputBuf minSize (Buffer fpbuf p0 op' ope)
                runStep nextStep buf'

            InsertByteString op' bs nextStep
              | S.null bs ->   -- flushing of buffer required
                  outputBuf 1 (Buffer fpbuf p0 op' ope) >>= runStep nextStep
              | p0 == op' -> do -- no bytes written: just insert bytestring
                  outputBS bs
                  runStep nextStep buf
              | otherwise -> do   -- bytes written, insert buffer and bytestring
                  buf' <- outputBuf 1 (Buffer fpbuf p0 op' ope)
                  outputBS bs
                  runStep nextStep buf'
{-# INLINE runPut #-}
