{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE BangPatterns  #-}

-- | A specialized decoder for MsgPack.
module Codec.MsgPack.Decoder where

import           Control.Applicative

import qualified Data.ByteString as S
import qualified Data.Text       as T
import           Foreign
import           GHC.Prim
import           GHC.Word

type MsgPackRead = Addr# -> (# Addr#, MsgPackValue #)

-- | The representation for a stream of values to be serialized.
data MsgPackValue
    = VArrayLen   {-# UNPACK #-} !Word
    | VWord64     {-# UNPACK #-} !Word64
    | VInt64      {-# UNPACK #-} !Int64
    | VChar       {-# UNPACK #-} !Char
    | VBool                      !Bool
    | VFloat      {-# UNPACK #-} !Float
    | VDouble     {-# UNPACK #-} !Double
    | VMapLen     {-# UNPACK #-} !Int
    | VTextLen    {-# UNPACK #-} !Int
    | VBinaryLen  {-# UNPACK #-} !Int
    | VText                      !T.Text
    | VBinary                    !S.ByteString
    | VNil
    | VEnd
    deriving (Show)

readNil, readFalse, readTrue :: MsgPackRead
readNil   ip# = (# ip# `plusAddr#` 1#, VNil #)
readFalse ip# = (# ip# `plusAddr#` 1#, VBool False #)
readTrue  ip# = (# ip# `plusAddr#` 1#, VBool True  #)


readPosFixnum :: MsgPackValue -> MsgPackRead
readPosFixnum i ip# = (# ip# `plusAddr#` 1#, i #)

readWord8 :: MsgPackRead
readWord8 ip# = case indexWord8OffAddr# ip# 1# of
    w# -> (# ip# `plusAddr#` 2#, VWord64 (fromIntegral $ W8# w#) #)


readWord16 :: MsgPackRead
readWord16 ip# =
    case indexWord8OffAddr# ip# 1# of
     w0# ->
      case indexWord8OffAddr# ip# 2# of
       w1# -> let !res = VWord64 $ fromIntegral $ w w0# `shiftL` 8 .|. w w1#
              in  (# ip# `plusAddr#` 3# , res #)
  where
    w w# = W# w#

readWord32 :: MsgPackRead
readWord32 ip# =
    case indexWord8OffAddr# ip# 1# of
     w0# ->
      case indexWord8OffAddr# ip# 2# of
       w1# ->
        case indexWord8OffAddr# ip# 3# of
         w2# ->
          case indexWord8OffAddr# ip# 4# of
           w3# -> let !res = VWord64 $ fromIntegral $ w w0# `shiftL` 24 .|.
                                                      w w1# `shiftL` 16 .|.
                                                      w w2# `shiftL`  8 .|.
                                                      w w3#
                  in (# ip# `plusAddr#` 5# , res  #)
  where
    w w# = W# w#


readWord64 :: MsgPackRead
readWord64 ip# =
    case indexWord8OffAddr# ip# 1# of
     w0# ->
      case indexWord8OffAddr# ip# 2# of
       w1# ->
        case indexWord8OffAddr# ip# 3# of
         w2# ->
          case indexWord8OffAddr# ip# 4# of
           w3# ->
            case indexWord8OffAddr# ip# 5# of
             w4# ->
              case indexWord8OffAddr# ip# 6# of
               w5# ->
                case indexWord8OffAddr# ip# 7# of
                 w6# ->
                  case indexWord8OffAddr# ip# 8# of
                   w7# -> let !res = VWord64 $ fromIntegral $
                                         w w0# `shiftL` 56 .|.
                                         w w1# `shiftL` 48 .|.
                                         w w2# `shiftL` 40 .|.
                                         w w3# `shiftL` 32 .|.
                                         w w4# `shiftL` 24 .|.
                                         w w5# `shiftL` 16 .|.
                                         w w6# `shiftL`  8 .|.
                                         w w7#
                          in (# ip# `plusAddr#` 9# , res  #)
  where
    w w# = W64# w#

{-
readWord64 :: MsgPackRead
readWord64 ip# =
    case indexWord8OffAddr# ip# 1# of
     w0# ->
      case indexWord8OffAddr# ip# 2# of
       w1# ->
        case indexWord8OffAddr# ip# 3# of
         w2# ->
          case indexWord8OffAddr# ip# 4# of
           w3# ->
            (# ip# `plusAddr#` 5#
            ,  VWord64 $ fromIntegral $ w w0# `shiftL` 24 .|.
                                        w w1# `shiftL` 16 .|.
                                        w w2# `shiftL`  8 .|.
                                        w w3#
            #)
  where
    w w# = W# w#

-}


{-
data Decoder a = Decoder
    { runDecoder :: forall r. (a -> InStream r) -> InStream r }

data InStream a
    = GetValue (MsgPackValue -> InStream a)
    | Fail String
    | Done a



-- instances
------------

instance Functor Decoder where
    {-# INLINE fmap #-}
    fmap f = \d -> Decoder $ \k -> runDecoder d (k . f)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure = \x -> Decoder $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> Decoder $ \k ->
                        runDecoder df (\f -> runDecoder dx (\x -> k (f x)))

instance Monad Decoder where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> Decoder $ \k -> runDecoder dm (\m -> runDecoder (f m) k)

    {-# INLINE (>>) #-}
    (>>) = \dm dn -> Decoder $ \k -> runDecoder dm (\_ -> runDecoder dn k)

    fail msg = Decoder $ \_ -> Fail msg



-- Decoder construction
------------------------------

wrongType :: String -> MsgPackValue -> InStream a
wrongType expected v =
    Fail $ "expected " ++ expected ++ ", but got " ++ show v

primD :: (forall r. (a -> InStream r) -> MsgPackValue -> InStream r) -> Decoder a
primD f = Decoder $ \k -> GetValue $ \v -> f k v

{-# INLINE nil #-}
nil :: Decoder ()
nil = primD $ \k v -> case v of VNil -> k ()
                                _    -> wrongType "nil" v


{-# INLINE bool #-}
bool :: Decoder Bool
bool = primD $ \k v -> case v of VBool x -> k x
                                 _       -> wrongType "bool" v

{-# INLINE float #-}
float :: Decoder Float
float = primD $ \k v -> case v of VFloat x -> k x
                                  _        -> wrongType "float" v

{-# INLINE double #-}
double :: Decoder Double
double = primD $ \k v -> case v of VDouble x -> k x
                                   _         -> wrongType "double" v

{-
{-# INLINE word #-}
word :: Decoder Word
word =

{-# INLINE word8 #-}
word8 :: Decoder Word8
word8 =

{-# INLINE word16 #-}
word16 :: Decoder Word16
word16 =

{-# INLINE word32 #-}
word32 :: Decoder Word32
word32 =

{-# INLINE word64 #-}
word64 :: Decoder Word64
word64 =

{-# INLINE int #-}
int :: Decoder Int
int = primD . OInt64 . fromIntegral

{-# INLINE int8 #-}
int8 :: Decoder Int8
int8 = primD . OInt64 . fromIntegral

{-# INLINE int16 #-}
int16 :: Decoder Int16
int16 = primD . OInt64 . fromIntegral

{-# INLINE int32 #-}
int32 :: Decoder Int32
int32 = primD . OInt64 . fromIntegral

{-# INLINE int64 #-}
int64 :: Decoder Int64
int64 = primD . OInt64

{-# INLINE binary #-}
binary :: Decoder S.ByteString
binary = primD . OBinary

{-# INLINE char #-}
char :: Decoder Char
char = primD . OChar

{-# INLINE text #-}
text :: Decoder T.Text
text = primD . OText

{-# INLINE arrayLen #-}
arrayLen :: Decoder Int
arrayLen = primD . OArrayLen . fromIntegral

{-# INLINE mapLen #-}
mapLen :: Decoder Int
mapLen = primD . OMapLen . fromIntegral
-}


{-
decodeWith :: DStream a -> S.ByteString -> Either String a
decodeWith ds0 (S.PS fpbuf off len) = S.inlinePerformIO $ do
    withForeignPtr fpbuf $ \pbuf -> do
      let !ip0 = pbuf `plusPtr` off
          !ipe = ip0 `plusPtr` len
          !ipeFast = ip0 `plusPtr` (len - 16) -- max size for fast reading

          err :: String -> Ptr Word8 -> IO (Either String a)
          err msg ip = return $ Left $ msg ++
              " (at byte " ++ show (ip `minusPtr` ip0) ++
              " of " ++ show len ++ ")"

          unexpectedEOI loc =
              err ("unexpected end-of-input while decoding " ++  loc)

          go :: Ptr Word8 -> DStreamRep a -> IO (Either String a)
          go !ip ds
            | ip < ipeFast = -- distinguish on stream action
                             -- lookup IO read action in array
                             -- read and get new 'ip', and, msg-pack value
                             -- call stream with value

            | otherwise    = -- distinguish on stream action
                             -- translate to slow mode if necessary
                             --

          case ds of
              DReturn x -> return $ Right x

              DFail msg -> err msg ip

              DWord8  k -> readN 1 $ \ip' -> do (W8# x) <- peek $ castPtr ip
                                                go ip' (k x)

              DWord16 k -> readN 2 $ \ip' -> do (W16# x) <- peek $ castPtr ip
                                                go ip' (k x)

              DWord32 k -> readN 4 $ \ip' -> do (W32# x) <- peek $ castPtr ip
                                                go ip' (k x)

              DWord64 k -> readN 8 $ \ip' -> do (W64# x) <- peek $ castPtr ip
                                                go ip' (k x)

              DWord k -> readN (sizeOf (undefined :: Word)) $ \ip' -> do
                  (W# x) <- peek $ castPtr ip
                  go ip' (k x)

              DInt8  k -> readN 1 $ \ip' -> do (I8# x) <- peek $ castPtr ip
                                               go ip' (k x)
              DInt16 k -> readN 2 $ \ip' -> do (I16# x) <- peek $ castPtr ip
                                               go ip' (k x)
              DInt32 k -> readN 4 $ \ip' -> do (I32# x) <- peek $ castPtr ip
                                               go ip' (k x)
              DInt64 k -> readN 8 $ \ip' -> do (I64# x) <- peek $ castPtr ip
                                               go ip' (k x)

              DInt k -> readN (sizeOf (undefined :: Int)) $ \ip' -> do
                  (I# x) <- peek $ castPtr ip
                  go ip' (k x)

              DFloat k -> readN (sizeOf (undefined :: Float)) $ \ip' -> do
                  (F# x) <- peek $ castPtr ip
                  go ip' (k x)

              DDouble k -> readN (sizeOf (undefined :: Double)) $ \ip' -> do
                  (D# x) <- peek $ castPtr ip
                  go ip' (k x)

              DChar k
                | ip `plusPtr` 4 <= ipe -> do
                    let peek8 = peekByteOff ip
                    w0 <- peek ip
                    case () of
                      _ | w0 < 0x80 -> do
                            let !c# = chr1# w0
                            go (ip `plusPtr` 1) (k c#)

                        | w0 < 0xe0 -> do
                            w1 <- peek8 1
                            let !c# = chr2# w0 w1
                            go (ip `plusPtr` 2) (k c#)

                        | w0 < 0xf0 -> do
                            w1 <- peek8 1; w2 <- peek8 2
                            let !c# = chr3# w0 w1 w2
                            go (ip `plusPtr` 3) (k c#)

                        | otherwise -> do
                            w1 <- peek8 1; w2 <- peek8 2; w3 <- peek8 3
                            let !c# = chr4# w0 w1 w2 w3
                            go (ip `plusPtr` 4) (k c#)

                | otherwise ->
                    go ip (unDStream (slowCharUtf8 ip) (\ !(C# c#) -> k c#))

              DSlowWord8 ipErr locErr k
                | ip < ipe  -> do x <- peek $ castPtr ip
                                  go (ip `plusPtr` 1) (k x)
                | otherwise -> unexpectedEOI locErr ipErr
            where
              {-# INLINE readN #-}
              readN :: Int
                    -> (Ptr Word8 -> IO (Either String a))
                    -> IO (Either String a)
              readN n io =
                  let ip' = ip `plusPtr` n in
                  if ip' <= ipe
                    then io ip'
                    else unexpectedEOI ("reading " ++ show n ++ " bytes") ip

      -- start the decoding
      go ip0 (unDStream ds0 DReturn)
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

slowCharUtf8 :: Ptr Word8 -> DStream Char
slowCharUtf8 ip = do
    w0 <- word8'
    case () of
      _ | w0 < 0x80 -> return (chr1 w0)
        | w0 < 0xe0 -> chr2 w0 <$> word8'
        | w0 < 0xf0 -> chr3 w0 <$> word8' <*> word8'
        | otherwise -> chr4 w0 <$> word8' <*> word8' <*> word8'
  where
    word8'           = slowWord8 ip "char (UTF-8)"
    chr1 w0          = C# (chr1# w0)
    chr2 w0 w1       = C# (chr2# w0 w1)
    chr3 w0 w1 w2    = C# (chr3# w0 w1 w2)
    chr4 w0 w1 w2 w3 = C# (chr4# w0 w1 w2 w3)

slowWord8 :: Ptr Word8 -> String -> DStream Word8
slowWord8 ip msg = DStream (\k -> DSlowWord8 ip msg k)

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

chr4# :: Word8 -> Word8 -> Word8 -> Word8 -> Char#
chr4# (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    (chr# (z1# +# z2# +# z3# +# z4#))
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
-}
-}
