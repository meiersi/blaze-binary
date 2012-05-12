{-# LANGUAGE CPP, UnboxedTuples, BangPatterns #-}
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
-- Stream based decoding of binary values.
--
-----------------------------------------------------------------------------
module Data.Blaze.Binary.StreamDecoding (
    benchWord8s
  ) where

import Data.Word
import Control.Applicative

import qualified Data.ByteString.Internal                            as S

import Foreign.Ptr        (plusPtr)
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.Storable   (peek)

#if  __GLASGOW_HASKELL__ >= 702
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#else
import Foreign.ForeignPtr        (unsafeForeignPtrToPtr)
#endif

------------------------------------------------------------------------

-- | The representation for a stream of values to be serialized.
data VStream =
     --  VChar           {-# UNPACK #-} !Char         VStreamRep
     -- | VWord           {-# UNPACK #-} !Word         VStreamRep
       VWord8          {-# UNPACK #-} !Word8        VStream
     -- | VWord16         {-# UNPACK #-} !Word16       VStreamRep
     -- | VWord32         {-# UNPACK #-} !Word32       VStreamRep
     -- | VWord64         {-# UNPACK #-} !Word64       VStreamRep
     -- | VInt            {-# UNPACK #-} !Int          VStreamRep
     -- | VInt8           {-# UNPACK #-} !Int8         VStreamRep
     -- | VInt16          {-# UNPACK #-} !Int16        VStreamRep
     -- | VInt32          {-# UNPACK #-} !Int32        VStreamRep
     -- | VInt64          {-# UNPACK #-} !Int64        VStreamRep
     -- | VFloat          {-# UNPACK #-} !Float        VStreamRep
     -- | VDouble         {-# UNPACK #-} !Double       VStreamRep
     -- | VInteger                       !Integer      VStreamRep
     -- | VByteString                    !S.ByteString VStreamRep
     -- | VBuilder                       !B.Builder    VStreamRep
     | VFail String
     | VEmpty

newtype Decoder a = Decoder { unDecoder :: VStream -> (# a, VStream #) }

toVStream :: S.ByteString -> VStream
toVStream (S.PS fpbuf off len) =
    go ip0
  where
    pbuf = unsafeForeignPtrToPtr fpbuf
    ip0  = pbuf `plusPtr` off
    ipe  = ip0 `plusPtr` len

    go !ip
      | ip < ipe = S.inlinePerformIO $ do
          w <- peek ip
          touchForeignPtr fpbuf
          return $ VWord8 w (go (ip `plusPtr` 1))
      | otherwise =
          VEmpty

runDecoder :: Decoder a -> S.ByteString -> Either String a
runDecoder d bs = case unDecoder d (toVStream bs) of
  (# _, VFail msg #) -> Left msg
  (# x, _         #) -> Right x

instance Functor Decoder where
  {-# INLINE fmap #-}
  fmap = \f d -> Decoder $ \vs0 -> case unDecoder d vs0 of
                                    (# x, vs1 #) -> (# f x, vs1 #)


instance Applicative Decoder where
  {-# INLINE pure #-}
  pure = \x -> Decoder $ \vs -> (# x, vs #)

  {-# INLINE (<*>) #-}
  (<*>) = \fd xd -> Decoder $ \vs0 ->
      case unDecoder fd vs0 of
        (# f, vs1 #) -> case unDecoder xd vs1 of
          (# x, vs2 #) -> (# f x, vs2 #)

instance Monad Decoder where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  (>>=) = \md f -> Decoder $ \vs0 ->
      case unDecoder md vs0 of
        (# _, vs1@(VFail _) #) -> (# error "impossible", vs1 #)
        (# m, vs1           #) -> unDecoder (f m) vs1

  -- We store the failure in the remainder of the stream to piggy-back failure
  -- detection on the pattern matching of the stream constructor.-
  fail msg = Decoder $ \_ -> (# error "Decoder:fail: impossible", VFail msg #)

{-# INLINE word8 #-}
word8 :: Decoder Word8
word8 = Decoder $ \vs0 -> case vs0 of
  VWord8 w vs1 -> (# w, vs1 #)
  _            -> (# error "impossible", VFail "expected Word8, but got something else" #)

word8s :: Decoder [Word8]
word8s = do
    tag <- word8
    case tag of
      0 -> return []
      1 -> (:) <$> word8 <*> word8s
      _ -> fail $ "word8s: unexpected tag " ++ show tag


benchWord8s :: S.ByteString -> [Word8]
benchWord8s bs = case runDecoder word8s bs of
    Left msg -> error msg
    Right x  -> x
