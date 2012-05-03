{-# LANGUAGE BangPatterns, DeriveDataTypeable, OverloadedStrings #-}
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

newtype Parser a = Parser { unParser :: Buffer -> IO (Res a) }

instance Functor Res where
    {-# INLINE fmap #-}
    fmap f (Res x ip) = Res (f x) ip

instance Functor Parser where
    fmap f = Parser . fmap (fmap (fmap f)) . unParser

instance Applicative Parser where
    {-# INLINE pure #-}
    pure x = Parser $ \(Buffer ip _) -> return (Res x ip)

    {-# INLINE (<*>) #-}
    Parser fIO <*> Parser xIO = Parser $ \ !buf@(Buffer _ ipe0) -> do
        Res f ip1 <- fIO buf
        Res x ip2 <- xIO (Buffer ip1 ipe0)
        evaluate (Res (f x) ip2)

instance Monad Parser where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    Parser xIO >>= f = Parser $ \ !buf@(Buffer _ ipe0) -> do
        Res x ip1 <- xIO buf
        unParser (f x) (Buffer ip1 ipe0)

    {-# INLINE fail #-}
    fail msg = Parser $ \(Buffer ip _) -> throw $ ParseException msg ip


requires :: Int -> Parser a -> Parser a
requires n p = Parser $ \buf@(Buffer ip ipe) ->
    if ipe `minusPtr` ip >= n
      then unParser p buf
      else throw $ (`ParseException` ip) $
             "required " ++ show n ++ 
             " bytes, but there are only " ++ show (ipe `minusPtr` ip) ++
             " bytes left."


{-# INLINE word8 #-}
word8 :: Parser Word8
word8 = Parser $ \(Buffer ip ipe) -> do
    let ip' = ip `plusPtr` 1
    if ip' < ipe
      then do x <- peek ip
              return (Res x ip')
      else throw $ (`ParseException` (ip' `plusPtr` (-1))) $
             "less than the one byte left"

word8sSimple :: Parser [Word8]
word8sSimple = do
    tag <- word8
    case tag of
      0 -> return []
      1 -> (:) <$> word8 <*> word8s
      _ -> fail $ "word8s: unexpected tag " ++ show tag

word8s :: Parser [Word8]
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

runParser :: Parser a -> S.ByteString -> Either String a
runParser p (S.PS fpbuf off len) = S.inlinePerformIO $ do
    withForeignPtr fpbuf $ \pbuf -> do
        let !ip  = pbuf `plusPtr` off
            !ipe = ip `plusPtr` len
        (`catch` handler) $ do
            Res x _ <- unParser p (Buffer ip ipe)
            return (Right x)
  where
    handler :: ParseException -> IO (Either String a)
    handler (ParseException msg _) = return $ Left msg

