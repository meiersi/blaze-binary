{-# LANGUAGE ScopedTypeVariables, BangPatterns, DeriveDataTypeable, OverloadedStrings #-}
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

import Data.Typeable
import qualified Data.ByteString.Internal as S

import Foreign 

data Res a = Res !a {-# UNPACK #-} !(Ptr Word8)

data Buffer = Buffer {-# UNPACK #-} !(Ptr Word8)  -- ^ First input byte
                     {-# UNPACK #-} !(Ptr Word8)  -- ^ First byte after

data ParseException = ParseException String {-# UNPACK #-} !(Ptr Word8)
  deriving( Show, Typeable )

instance Exception ParseException where

newtype Decoder a = Decoder { unDecoder :: Buffer -> IO (Res a) }

instance Functor Res where
    {-# INLINE fmap #-}
    fmap f (Res x ip) = Res (f x) ip

instance Functor Decoder where
    fmap f = Decoder . fmap (fmap (fmap f)) . unDecoder

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure x = Decoder $ \(Buffer ip _) -> return (Res x ip)

    {-# INLINE (<*>) #-}
    Decoder fIO <*> Decoder xIO = Decoder $ \ !buf@(Buffer _ ipe0) -> do
        Res f ip1 <- fIO buf
        Res x ip2 <- xIO (Buffer ip1 ipe0)
        evaluate (Res (f x) ip2)

instance Monad Decoder where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    Decoder xIO >>= f = Decoder $ \ !buf@(Buffer _ ipe0) -> do
        Res x ip1 <- xIO buf
        unDecoder (f x) (Buffer ip1 ipe0)

    {-# INLINE fail #-}
    fail msg = Decoder $ \(Buffer ip _) -> throw $ ParseException msg ip


requires :: Int -> Decoder a -> Decoder a
requires n p = Decoder $ \buf@(Buffer ip ipe) ->
    if ipe `minusPtr` ip >= n
      then unDecoder p buf
      else throw $ (`ParseException` ip) $
             "required " ++ show n ++ 
             " bytes, but there are only " ++ show (ipe `minusPtr` ip) ++
             " bytes left."

{-# INLINE storable #-}
storable :: forall a. Storable a => Decoder a
storable = Decoder $ \(Buffer ip ipe) -> do
    let ip' = ip `plusPtr` size
    if ip' <= ipe
      then do x <- peek (castPtr ip)
              return (Res x ip')
      else throw $ (`ParseException` (ip' `plusPtr` negate size)) $
             "less than the required " ++ show size ++ " bytes left."
  where
    size = sizeOf (undefined :: a)

runDecoder :: Decoder a -> S.ByteString -> Either String a
runDecoder p (S.PS fpbuf off len) = S.inlinePerformIO $ do
    withForeignPtr fpbuf $ \pbuf -> do
        let !ip  = pbuf `plusPtr` off
            !ipe = ip `plusPtr` len
        (`catch` handler) $ do
            Res x _ <- unDecoder p (Buffer ip ipe)
            return (Right x)
  where
    handler :: ParseException -> IO (Either String a)
    handler (ParseException msg _) = return $ Left msg

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

