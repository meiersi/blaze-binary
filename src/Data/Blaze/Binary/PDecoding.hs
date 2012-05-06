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
-- Decoding of binary values parametrized over the primitive parsers.
--
-----------------------------------------------------------------------------
module Data.Blaze.Binary.PDecoding where

import Prelude hiding (catch)

import qualified Data.Blaze.Binary.Decoding as D

import Control.Applicative
import Control.Exception

import Data.Typeable
import qualified Data.ByteString.Internal as S
import GHC.Prim
import GHC.Ptr
import GHC.Word
import GHC.Exts
import GHC.IO (IO(IO))
import Foreign 

data ParseException = ParseException String -- {-# UNPACK #-} !(Ptr Word8)
  deriving( Show, Typeable )

instance Exception ParseException where

data PrimDecoders = PD {
       pdWord8      :: D.Decoder Word8
     , pdWord16     :: D.Decoder Word16
     , pdWord32     :: D.Decoder Word32
     , pdWord64     :: D.Decoder Word64
     , pdWord       :: D.Decoder Word
     , pdInt8       :: D.Decoder Int8
     , pdInt16      :: D.Decoder Int16
     , pdInt32      :: D.Decoder Int32
     , pdInt64      :: D.Decoder Int64
     , pdInt        :: D.Decoder Int
     , pdFloat      :: D.Decoder Float
     , pdDouble     :: D.Decoder Double
     , pdChar       :: D.Decoder Char
     , pdByteString :: Int -> D.Decoder S.ByteString
     }

defaultPDs :: PrimDecoders
defaultPDs = PD D.word8 D.word16 D.word32 D.word64 D.word
                D.int8  D.int16  D.int32  D.int64  D.int
                D.float D.double D.char
                D.byteStringSlice
        

------------------------------------------------------------------------------
-- Decoder
------------------------------------------------------------------------------

newtype Decoder a = Decoder { 
          unDecoder :: PrimDecoders
                    -> ForeignPtr Word8 -> Addr# -> Addr# 
                    -> State# RealWorld -> (# State# RealWorld, Addr#, a #)
        }

instance Functor Decoder where
    fmap f = \(Decoder io) -> Decoder $ \pd fpbuf ip0 ipe s0 -> 
        case io pd fpbuf ip0 ipe s0 of
            (# s1, ip1, x #) -> (# s1, ip1, f x #)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure x = Decoder $ \_ _ ip0 _ s0 -> (# s0, ip0, x #)

    {-# INLINE (<*>) #-}
    Decoder fIO <*> Decoder xIO = Decoder $ \pd fpbuf ip0 ipe s0 ->
        case fIO pd fpbuf ip0 ipe s0 of
          (# s1, ip1, f #) -> case xIO pd fpbuf ip1 ipe s1 of
            (# s2, ip2, x #) -> (# s2, ip2, f x #)

{-# INLINE liftIO #-}
liftIO :: IO a -> Decoder a
liftIO (IO io) = Decoder $ \_ _ ip0 _ s0 -> case io s0 of
  (# s1, x #) -> (# s1, ip0, x #)

{-# INLINE runIO #-}
runIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
runIO (IO io) = io

instance Monad Decoder where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    Decoder xIO >>= f = Decoder $ \pd fpbuf ip0 ipe s0 ->
        case xIO pd fpbuf ip0 ipe s0 of
          (# s1, ip1, x #) -> unDecoder (f x) pd fpbuf ip1 ipe s1

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

{-# INLINE prim #-}
prim :: (PrimDecoders -> D.Decoder a) -> Decoder a
prim sel = Decoder $ \pd fpbuf ip0 ipe s0 ->
    D.unDecoder (sel pd) fpbuf ip0 ipe s0


runDecoder :: Decoder a -> S.ByteString -> Either String a
runDecoder p (S.PS fpbuf off len) = S.inlinePerformIO $ do
    withForeignPtr fpbuf $ \pbuf -> do
        let !(Ptr ip)  = pbuf `plusPtr` off
            !(Ptr ipe) = Ptr ip `plusPtr` len
        (`catch` handler) $ do
            x <- IO $ \s0 -> case unDecoder p defaultPDs fpbuf ip ipe s0 of
                               (# s1, _, x #) -> (# s1, x #)
            return (Right x)
  where
    handler :: ParseException -> IO (Either String a)
    handler (ParseException msg) = return $ Left msg

-- Primitive parsers
--------------------

{-# INLINE word8 #-}
word8 :: Decoder Word8
word8 = prim pdWord8

{-# INLINE word16 #-}
word16 :: Decoder Word16
word16 = prim pdWord16

{-# INLINE word32 #-}
word32 :: Decoder Word32
word32 = prim pdWord32

{-# INLINE word64 #-}
word64 :: Decoder Word64
word64 = prim pdWord64

{-# INLINE word #-}
word :: Decoder Word
word = prim pdWord

{-# INLINE int8 #-}
int8 :: Decoder Int8
int8 = prim pdInt8

{-# INLINE int16 #-}
int16 :: Decoder Int16
int16 = prim pdInt16

{-# INLINE int32 #-}
int32 :: Decoder Int32
int32 = prim pdInt32

{-# INLINE int64 #-}
int64 :: Decoder Int64
int64 = prim pdInt64

{-# INLINE int #-}
int :: Decoder Int
int = prim pdInt

{-# INLINE float #-}
float :: Decoder Float
float = prim pdFloat

{-# INLINE double #-}
double :: Decoder Double
double = prim pdDouble

{-# INLINE byteString #-}
byteString :: Int -> Decoder S.ByteString
byteString = \len -> prim (`pdByteString` len)

char :: Decoder Char
char = prim pdChar

{-# INLINE getAddr #-}
getAddr :: Ptr a -> Addr#
getAddr (Ptr a) = a

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
decodeMaybe just = 
    go
  where 
    go = do tag <- word8
            case tag of
              0 -> return Nothing
              1 -> Just <$> just
              _ -> fail $ "decodeMaybe: unexpected tag " ++ show tag

{-# INLINE decodeEither #-}
decodeEither :: Decoder a -> Decoder b -> Decoder (Either a b)
decodeEither left right = 
    go
  where 
    go = do tag <- word8
            case tag of
              0 -> Left <$> left 
              1 -> Right <$> right
              _ -> fail $ "decodeEither: unexpected tag " ++ show tag


