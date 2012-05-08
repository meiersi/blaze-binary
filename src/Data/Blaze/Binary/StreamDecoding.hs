{-# LANGUAGE MagicHash, RankNTypes, BangPatterns #-}
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
-- Stream decoding of binary values.
--
-----------------------------------------------------------------------------
module Data.Blaze.Binary.StreamDecoding where

import Prelude hiding (catch)

import Control.Applicative

import qualified Data.ByteString.Internal as S

import Foreign 
import GHC.Word
import GHC.Prim

data DStreamRep a = 
       DReturn a
     | DFail String
     | DWord8 (Word# -> DStreamRep a)

newtype DStream a = DStream { 
          unDStream :: forall r. (a -> DStreamRep r) -> DStreamRep r
        }

instance Functor DStream where
    {-# INLINE fmap #-}
    fmap f = \s -> DStream $ \k -> unDStream s (k . f)
 
instance Applicative DStream where
    {-# INLINE pure #-}
    pure = \x -> DStream $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \sf sx -> DStream $ \k ->
                        unDStream sf (\f -> unDStream sx (\x -> k (f x)))

instance Monad DStream where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \sm f -> DStream $ \k -> unDStream sm (\m -> unDStream (f m) k)

    fail msg = DStream $ \_ -> DFail msg

word8 :: DStream Word8
word8 = DStream $ \k -> DWord8 (\x -> k (W8# x))

word8s :: DStream [Word8]
word8s = do
    tag <- word8
    case tag of
      0 -> return []
      1 -> (:) <$> word8 <*> word8s
      _ -> fail $ "word8s: unexpected tag " ++ show tag

decodeWith :: DStream a -> S.ByteString -> Either String a
decodeWith ds0 (S.PS fpbuf off len) = S.inlinePerformIO $ do
    withForeignPtr fpbuf $ \pbuf -> do
      let !ip0 = pbuf `plusPtr` off
          !ipe = ip0 `plusPtr` len

          err :: String -> Ptr Word8 -> IO (Either String a)
          err msg ip = return $ Left $ msg ++ 
              " (at byte " ++ show (ip `minusPtr` ip0) ++ 
              " of " ++ show len ++ ")" 

          unexpectedEOI = err "unexpected end-of-input"

          go :: Ptr Word8 -> DStreamRep a -> IO (Either String a)
          go !ip !ds = case ds of
              DReturn x -> return $ Right x
              DFail msg -> err msg ip
              DWord8 k 
                | ip < ipe  -> do (W8# w) <- peek ip
                                  go (ip `plusPtr` 1) (k w)
                | otherwise -> unexpectedEOI ip

      go ip0 (unDStream ds0 DReturn)



{-
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

-}
