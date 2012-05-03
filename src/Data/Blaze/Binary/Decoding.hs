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
-- Binary encoding of values.
--
-----------------------------------------------------------------------------
module Data.Blaze.Binary.Decoding where

import Prelude hiding (catch)

import Control.Applicative
import Control.Exception

import Data.Typeable
import qualified Data.ByteString.Internal as S
import GHC.Prim
import GHC.Ptr
import GHC.Exts
import GHC.IO (IO(IO))
import Foreign 

data ParseException = ParseException String -- {-# UNPACK #-} !(Ptr Word8)
  deriving( Show, Typeable )

instance Exception ParseException where

newtype Decoder a = Decoder { 
          unDecoder :: Addr# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, a #)
        }

instance Functor Decoder where
    fmap f = \(Decoder io) -> Decoder $ \ip0 ipe0 s0 -> case io ip0 ipe0 s0 of
      (# s1, ip1, x #) -> (# s1, ip1, f x #)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure x = Decoder $ \ip0 _ s0 -> (# s0, ip0, x #)

    {-# INLINE (<*>) #-}
    Decoder fIO <*> Decoder xIO = Decoder $ \ip0 ipe0 s0 ->
        case fIO ip0 ipe0 s0 of
          (# s1, ip1, f #) -> case xIO ip1 ipe0 s1 of
            (# s2, ip2, x #) -> (# s2, ip2, f x #)

{-# INLINE liftIO #-}
liftIO :: IO a -> Decoder a
liftIO (IO io) = Decoder $ \ip0 _ s0 -> case io s0 of
  (# s1, x #) -> (# s1, ip0, x #)

{-# INLINE runIO #-}
runIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
runIO (IO io) = io

instance Monad Decoder where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    Decoder xIO >>= f = Decoder $ \ip0 ipe0 s0 ->
        case xIO ip0 ipe0 s0 of
          (# s1, ip1, x #) -> unDecoder (f x) ip1 ipe0 s1

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
storable = Decoder $ \ip0 ipe0 s0 ->
    let ip1 = plusAddr# ip0 size in 
      if Ptr ip1 <= Ptr ipe0
        then case runIO (peek (Ptr ip0)) s0 of
               (# s1, x #) -> (# s1, ip1, x #)
        else unDecoder 
                (fail $ "less than the required " ++ show (I# size) ++ " bytes left.")
                ip0 ipe0 s0
  where
    !(I# size) = sizeOf (undefined :: a)

runDecoder :: Decoder a -> S.ByteString -> Either String a
runDecoder p (S.PS fpbuf off len) = S.inlinePerformIO $ do
    withForeignPtr fpbuf $ \pbuf -> do
        let !(Ptr ip)  = pbuf `plusPtr` off
            !(Ptr ipe) = Ptr ip `plusPtr` len
        (`catch` handler) $ do
            x <- IO $ \s0 -> case unDecoder p ip ipe s0 of
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
decodeMaybe x = 
    go
  where 
    go = do tag <- word8
            case tag of
              0 -> return Nothing
              1 -> Just <$> x
              _ -> fail $ "decodeMaybe: unexpected tag " ++ show tag


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

