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
-- Iteratee-style decoding of binary values.
--
-----------------------------------------------------------------------------
module Data.Blaze.Binary.Decoder (

    -- * The Decoder Type
      Decoder
    , runDecoder

    -- ** Primitive values
    , word
    , word8
    , word16
    , word32
    , word64

    , int
    , int8
    , int16
    , int32
    , int64

    , integer
    , float
    , double

    , char

    , byteString

    -- ** Combinators
    , decodeMaybe
    , decodeEither
    , decodeList

    -- ** Testing
    , word8s
    , string
    , listOfWord8s

  ) where

import Prelude hiding (catch)

import Control.Applicative
import Control.Monad (replicateM)

import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S

import Foreign
import GHC.Word
import GHC.Int
import GHC.Prim
import GHC.Types

------------------------------------------------------------------------------
-- Decoders
------------------------------------------------------------------------------

-- | A 'Decoder' for Haskell values.
newtype Decoder a = Decoder {
          unDecoder :: forall r. (a -> DStream r) -> DStream r
        }

-- | A 'DStream' is a stream of requests for parsing primitive values.
data DStream a =
       DWord8                          (Word#        -> DStream a)
     | DInt                            (Int#         -> DStream a)
     | DChar                           (Char#        -> DStream a)
     | DByteString                     (S.ByteString -> DStream a)
     | DWord                           (Word#        -> DStream a)
     | DFloat                          (Float#       -> DStream a)
     | DDouble                         (Double#      -> DStream a)
     | DWord64                         (Word#        -> DStream a)
     | DInt64                          (Int#         -> DStream a)
     | DWord32                         (Word#        -> DStream a)
     | DInt32                          (Int#         -> DStream a)
     | DWord16                         (Word#        -> DStream a)
     | DInt16                          (Int#         -> DStream a)
     | DInt8                           (Int#         -> DStream a)
     | DInteger                        (Integer      -> DStream a)
     | DSlowWord8  (Ptr Word8) String  (Word8 -> DStream a)
       -- ^ For reading a multi-byte primitive at the boundary.
     | DFail       String
     | DReturn a


-- Instances
------------

instance Functor Decoder where
    {-# INLINE fmap #-}
    fmap = \f s -> Decoder $ \k -> unDecoder s (k . f)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure = \x -> Decoder $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \sf sx ->
        Decoder $ \k -> unDecoder sf (\f -> unDecoder sx (\x -> k (f x)))

    {-# INLINE (*>) #-}
    (*>) = \sx sy -> Decoder $ \k -> unDecoder sx (\_ -> unDecoder sy k)

    {-# INLINE (<*) #-}
    (<*) = \sx sy ->
        Decoder $ \k -> unDecoder sx (\x -> unDecoder sy (\_ -> k x))

instance Monad Decoder where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \sm f -> Decoder $ \k -> unDecoder sm (\m -> unDecoder (f m) k)

    {-# INLINE (>>) #-}
    (>>) = (*>)

    fail msg = Decoder $ \_ -> DFail msg

------------------------------------------------------------------------------
-- Bounded decoders
------------------------------------------------------------------------------

data BoundedDecoder a = BoundedDecoder
       { bdBound :: {-# UNPACK #-} !Int
       , bdName  :: String
       , bdFast  :: forall r.
                       (String -> IO r)          -- failure
                    -> (Ptr Word8 -> a -> IO r)  -- success
                    -> Ptr Word8 -> IO r         -- IO-based decoder
       , bdSlow  :: Decoder Word8 -> Decoder a
       }

instance Functor BoundedDecoder where
    {-# INLINE fmap #-}
    fmap = \f (BoundedDecoder n name fast slow) ->
      BoundedDecoder n name
        (\failure success -> fast failure (\ip' -> success ip' . f))
        (\d8 -> f <$> slow d8)

{-# INLINE bdWord16BE #-}
bdWord16BE :: BoundedDecoder Word16
bdWord16BE = BoundedDecoder 2 "Word16 (little-endian)"
    (\_failure success ip -> do
        let p8 = peekByteOff ip
        w0 <- p8 0; w1 <- p8 1

        let x = fromIntegral (w0 :: Word8) `shiftL` 8 .|. fromIntegral w1
        success (ip `plusPtr` 4) x
    )
    (\d8 -> do w0 <- d8; w1 <- d8
               return $! fromIntegral w0 `shiftL` 8 .|. fromIntegral w1
    )

{-# INLINE bdWord32BE #-}
bdWord32BE :: BoundedDecoder Word32
bdWord32BE = BoundedDecoder 4 "Word32 (little-endian)"
    (\_failure success ip -> do
        let p8 = peekByteOff ip
        w0 <- p8 0; w1 <- p8 1; w2 <- p8 2; w3 <- p8 3;

        let x = at 0 w0 .|.  at 1 w1 .|.  at 2 w2 .|.  at 3 w3
        success (ip `plusPtr` 4) x
    )
    (\d8 -> do
        w0 <- d8; w1 <- d8; w2 <- d8; w3 <- d8
        return $! at 0 w0 .|.  at 1 w1 .|.  at 2 w2 .|.  at 3 w3
    )
  where
    {-# INLINE at #-}
    at n x = fromIntegral (x :: Word8) `shiftL` (n * 8)

{-# INLINE bdWord64BE #-}
bdWord64BE :: BoundedDecoder Word64
bdWord64BE = BoundedDecoder 8 "Word64 (little-endian)"
    (\_failure success ip -> do
        let p8 = peekByteOff ip
        w0 <- p8 0; w1 <- p8 1; w2 <- p8 2; w3 <- p8 3;
        w4 <- p8 4; w5 <- p8 5; w6 <- p8 6; w7 <- p8 7;

        let !x = at 0 w0 .|.  at 1 w1 .|.  at 2 w2 .|.  at 3 w3 .|.
                 at 4 w4 .|.  at 5 w5 .|.  at 6 w6 .|.  at 7 w7
        success (ip `plusPtr` 8) x
    )
    (\d8 -> do
        w0 <- d8; w1 <- d8; w2 <- d8; w3 <- d8
        w4 <- d8; w5 <- d8; w6 <- d8; w7 <- d8
        return $! at 0 w0 .|.  at 1 w1 .|.  at 2 w2 .|.  at 3 w3 .|.
                  at 4 w4 .|.  at 5 w5 .|.  at 6 w6 .|.  at 7 w7
    )
  where
    {-# INLINE at #-}
    at n x = fromIntegral (x :: Word8) `shiftL` (n * 8)

{-# INLINE bdWord16LE #-}
bdWord16LE :: BoundedDecoder Word16
bdWord16LE = BoundedDecoder 2 "Word16 (little-endian)"
    (\_failure success ip -> do
        x <- peek (castPtr ip :: Ptr Word16)
        success (ip `plusPtr` 2) x
    )
    (\d8 -> do w0 <- d8
               w1 <- d8
               return $! fromIntegral w1 `shiftL` 8 .|. fromIntegral w0
    )

{-# INLINE bdWord32LE #-}
bdWord32LE :: BoundedDecoder Word32
bdWord32LE = BoundedDecoder 4 "Word32 (little-endian)"
    (\_failure success ip -> do
        x <- peek (castPtr ip :: Ptr Word32)
        success (ip `plusPtr` 4) x
    )
    (\d8 -> do
        w0 <- d8; w1 <- d8; w2 <- d8; w3 <- d8
        return $! at 3 w0 .|.  at 2 w1 .|.  at 1 w2 .|.  at 0 w3
    )
  where
    {-# INLINE at #-}
    at n x = fromIntegral x `shiftL` (n * 8)

{-# INLINE bdWord64LE #-}
bdWord64LE :: BoundedDecoder Word64
bdWord64LE = BoundedDecoder 8 "Word64 (little-endian)"
    (\_failure success ip -> do
        x <- peek (castPtr ip :: Ptr Word64)
        success (ip `plusPtr` 8) x
    )
    (\d8 -> do
        w0 <- d8; w1 <- d8; w2 <- d8; w3 <- d8
        w4 <- d8; w5 <- d8; w6 <- d8; w7 <- d8
        return $! at 7 w0 .|.  at 6 w1 .|.  at 5 w2 .|.  at 4 w3 .|.
                  at 3 w4 .|.  at 2 w5 .|.  at 1 w6 .|.  at 0 w7
    )
  where
    {-# INLINE at #-}
    at n x = fromIntegral x `shiftL` (n * 8)


bdCharUtf8 :: BoundedDecoder Char
bdCharUtf8 = BoundedDecoder 4 "Char (UTF-8)"
    (\failure success ip ->  do
        let peek8 = peekByteOff ip
        w0 <- peek ip
        case () of
          _ | w0 < 0x80 -> do
                success (ip `plusPtr` 1) (chr1 w0)

            | w0 < 0xe0 -> do
                w1 <- peek8 1
                success (ip `plusPtr` 2) (chr2 w0 w1)

            | w0 < 0xf0 -> do
                w1 <- peek8 1; w2 <- peek8 2
                success (ip `plusPtr` 3) (chr3 w0 w1 w2)

            | otherwise -> do
                w1 <- peek8 1; w2 <- peek8 2; w3 <- peek8 3
                let x = chr4 w0 w1 w2 w3
                if x <= 0x10ffff
                  then success (ip `plusPtr` 4) (unsafeChr x)
                  else failure $ "Invalid Unicode codepoint '" ++ show  x
    )
    (\d8 -> do
        w0 <- d8
        case () of
          _ | w0 < 0x80 -> return (chr1 w0)
            | w0 < 0xe0 -> chr2 w0 <$> d8
            | w0 < 0xf0 -> chr3 w0 <$> d8 <*> d8
            | otherwise -> do
                x <- chr4 w0 <$> d8 <*> d8 <*> d8
                if x <= 0x10ffff
                  then return (unsafeChr x)
                  else fail $ "Invalid Unicode codepoint '" ++ show  x
    )
  where
    chr1 w0          = C# (chr1# w0)
    chr2 w0 w1       = C# (chr2# w0 w1)
    chr3 w0 w1 w2    = C# (chr3# w0 w1 w2)
    chr4 w0 w1 w2 w3 = I# (chr4# w0 w1 w2 w3)
    unsafeChr (I# i) = C# (chr# i)

------------------------------------------------------------------------------
-- Decoders for primitive values
------------------------------------------------------------------------------

word8 :: Decoder Word8
word8 = Decoder $ \k -> DWord8 (\x -> k (W8# x))

word16 :: Decoder Word16
word16 = Decoder $ \k -> DWord16 (\x -> k (W16# x))

word32 :: Decoder Word32
word32 = Decoder $ \k -> DWord32 (\x -> k (W32# x))

word64 :: Decoder Word64
word64 = Decoder $ \k -> DWord64 (\x -> k (W64# x))

word :: Decoder Word
word = Decoder $ \k -> DWord (\x -> k (W# x))

int8 :: Decoder Int8
int8 = Decoder $ \k -> DInt8 (\x -> k (I8# x))

int16 :: Decoder Int16
int16 = Decoder $ \k -> DInt16 (\x -> k (I16# x))

int32 :: Decoder Int32
int32 = Decoder $ \k -> DInt32 (\x -> k (I32# x))

int64 :: Decoder Int64
int64 = Decoder $ \k -> DInt64 (\x -> k (I64# x))

int :: Decoder Int
int = Decoder $ \k -> DInt (\x -> k (I# x))

integer :: Decoder Integer
integer = Decoder $ \k -> DInteger k

float :: Decoder Float
float = Decoder $ \k -> DFloat (\x -> k (F# x))

double :: Decoder Double
double = Decoder $ \k -> DDouble (\x -> k (D# x))

byteString :: Decoder S.ByteString
byteString = Decoder $ \k -> DByteString k

-- | Decode one 'Char'.
char :: Decoder Char
char = Decoder $ \k -> DChar (\x -> k (C# x))

word8s :: Decoder [Word8]
word8s = decodeList word8

string :: Decoder String
string = decodeList char

listOfWord8s :: Decoder [[Word8]]
listOfWord8s = decodeList (decodeList word8)

------------------------------------------------------------------------------
-- Decoder combinators
------------------------------------------------------------------------------

decodeMaybe :: Decoder a -> Decoder (Maybe a)
decodeMaybe just =
    go
  where
    go = do tag <- word8
            case tag of
              0 -> return Nothing
              1 -> Just <$> just
              _ -> fail $ "decodeMaybe: unexpected tag " ++ show tag

decodeEither :: Decoder a -> Decoder b -> Decoder (Either a b)
decodeEither left right =
    go
  where
    go = do tag <- word8
            case tag of
              0 -> Left <$> left
              1 -> Right <$> right
              _ -> fail $ "decodeEither: unexpected tag " ++ show tag


-- | Decode a list of values that were encoded with their size prefixed.
decodeList :: Decoder a -> Decoder [a]
decodeList decode = do n <- int
                       replicateM n decode

-- | Decode a list of values that were encoded in reverse order and with their
-- size prefixed.
-- decodeList :: Decoder a -> Decoder [a]
-- decodeList decode =
--     int >>= go []
--   where
--     go xs !n
--       | n <= 0    = return xs
--       | otherwise = do x <- decode; go (x:xs) (n - 1)

 --
 --    int >>= go
 --  where
 --    go n | n <= 0    = return []
 --         | otherwise = (:) <$> decode <*> go (n-1)

-- decodeList :: Decoder a -> Decoder [a]
-- decodeList decode =
--     go
--   where
--     go = do
--         tag <- word8
--         case tag of
--           0 -> return []
--           1 -> force ((:) <$> decode <*> go)
--           _ -> fail $ "decodeList: unexpected tag " ++ show tag

-- | Use 'force' to ensure that the finally returned value is in WHNF. This
-- reduces memory usage, as it flattens all the one-argument PAPS that were
-- built up. Note that flattening too early may result in an increased
-- runtime, as then some arguments are copied multiple times.
{-# INLINE force #-}
force :: Decoder a -> Decoder a
force ds = Decoder $ \k -> unDecoder ds (\x -> x `seq` (k x))

runDecoder :: Decoder a -> S.ByteString -> Either String a
runDecoder ds0 (S.PS fpbuf off len) = S.inlinePerformIO $ do
    withForeignPtr fpbuf $ \pbuf -> do
      let !ip0 = pbuf `plusPtr` (off + 4)
          !ipe = ip0 `plusPtr` (len - 4)

          err :: String -> Ptr Word8 -> IO (Either String a)
          err msg ip = return $ Left $ msg ++
              " (at byte " ++ show (ip `minusPtr` ip0) ++
              " of " ++ show len ++ ")"

          unexpectedEOI loc =
              err ("unexpected end-of-input while decoding " ++  loc)

          go :: Ptr Word8 -> DStream a -> IO (Either String a)
          go !ip ds = case ds of
              DReturn x -> return $ Right x

              DFail msg -> err msg ip

              DWord8  k
                | ip < ipe  -> do (W8# x) <- peek $ castPtr ip
                                  go (ip `plusPtr` 1) (k x)
                | otherwise -> unexpectedEOI "Word8" ip

              DInt k -> runBD (fromIntegral <$> bdWord64BE)
                      (\ip' !(I# x#) -> go ip' (k x#))
                      (\    !(I# x#) -> k x#         )
              -- DInt k -> readN (sizeOf (undefined :: Int)) $ \ip' -> do
                  -- (I# x) <- peek $ castPtr ip
                  -- go ip' (k x)

              DWord k -> runBD (fromIntegral <$> bdWord64BE)
                      (\ip' !(W# x#) -> go ip' (k x#))
                      (\    !(W# x#) -> k x#         )

              DWord16 k -> runBD bdWord16BE
                      (\ip' !(W16# x#) -> go ip' (k x#))
                      (\    !(W16# x#) -> k x#         )

              DWord32 k -> runBD bdWord32BE
                      (\ip' !(W32# x#) -> go ip' (k x#))
                      (\    !(W32# x#) -> k x#         )

              DWord64 k -> runBD bdWord64BE
                      (\ip' !(W64# x#) -> go ip' (k x#))
                      (\    !(W64# x#) -> k x#         )

              DInt16 k -> runBD (fromIntegral <$> bdWord16BE)
                      (\ip' !(I16# x#) -> go ip' (k x#))
                      (\    !(I16# x#) -> k x#         )

              DInt32 k -> runBD (fromIntegral <$> bdWord32BE)
                      (\ip' !(I32# x#) -> go ip' (k x#))
                      (\    !(I32# x#) -> k x#         )

              DInt64 k -> runBD (fromIntegral <$> bdWord64BE)
                      (\ip' !(I64# x#) -> go ip' (k x#))
                      (\    !(I64# x#) -> k x#         )


{-
              DFloat k -> readN (sizeOf (undefined :: Float)) $ \ip' -> do
                  (F# x) <- peek $ castPtr ip
                  go ip' (k x)

              DDouble k -> readN (sizeOf (undefined :: Double)) $ \ip' -> do
                  (D# x) <- peek $ castPtr ip
                  go ip' (k x)

-}
              DChar k ->
                runBD bdCharUtf8
                      (\ip' !(C# c#) -> go ip' (k c#))
                      (\    !(C# c#) -> k c#         )

              DSlowWord8 ipErr locErr k
                | ip < ipe  -> do x <- peek ip
                                  go (ip `plusPtr` 1) (k x)
                | otherwise -> unexpectedEOI locErr ipErr
            where
              {-# INLINE runBD #-}
              runBD :: BoundedDecoder b
                    -> (Ptr Word8 -> b -> IO (Either String a))
                    -> (b -> DStream a)
                    -> IO (Either String a)
              runBD (BoundedDecoder n name fast slow) io k
                | ip `plusPtr` n <= ipe = fast (`err` ip) io ip
                | otherwise = go ip (unDecoder (slow (slowWord8 ip name)) k)


      -- start the decoding
      go ip0 (unDecoder ds0 DReturn)

{-
{-# INLINE fastCharUtf8 #-}
fastCharUtf8 :: Ptr Word8 -> State# RealWorld -> (# State RealWorld, Char# #)
fastCharUtf8 ip = \s0 ->
  case runIO (peek ip0) s0 of
    (# s1, w0 #)
      | w0 < 0x80 -> (# s1, chr1 w0 #)

      | w0 < 0xe0 ->
          case runIO (peekByteOff ip0 1) s1 of
            (# s2, w1 #) -> (# s2, chr2 w0 w1 #)

      | w0 < 0xf0 ->
          case runIO (peekByteOff ip0 1) s1 of
            (# s2, w1 #) -> case runIO (peekByteOff ip0 2) s2 of
              (# s3, w2 #) -> (# s3, chr3 w0 w1 w2 #)

      | otherwise ->
          case runIO (peekByteOff ip0 1) s1 of
            (# s2, w1 #) -> case runIO (peekByteOff ip0 2) s2 of
              (# s3, w2 #) -> case runIO (peekByteOff ip0 3) s3 of
                (# s4, w3 #) -> (# s4, chr4 w0 w1 w2 w3 #)
-}

slowWord8 :: Ptr Word8 -> String -> Decoder Word8
slowWord8 ip msg = Decoder (\k -> DSlowWord8 ip msg k)

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

------------------------------------------------------------------------------
-- UTF-8 decoding helpers
------------------------------------------------------------------------------

chr1# :: Word8 -> Char#
chr1# (W8# x#) = (chr# (word2Int# x#))
{-# INLINE chr1# #-}

chr2# :: Word8 -> Word8 -> Char#
chr2# (W8# x1#) (W8# x2#) =
    (chr# (z1# +# z2#))
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
    !z2# = y2# -# 0x80#
{-# INLINE chr2# #-}

chr3# :: Word8 -> Word8 -> Word8 -> Char#
chr3# (W8# x1#) (W8# x2#) (W8# x3#) =
    (chr# (z1# +# z2# +# z3#))
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !y3# = word2Int# x3#
    !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
    !z3# = y3# -# 0x80#
{-# INLINE chr3# #-}

chr4# :: Word8 -> Word8 -> Word8 -> Word8 -> Int#
chr4# (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    (z1# +# z2# +# z3# +# z4#)
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !y3# = word2Int# x3#
    !y4# = word2Int# x4#
    !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
    !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
    !z4# = y4# -# 0x80#
{-# INLINE chr4# #-}
