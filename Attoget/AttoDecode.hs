{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- | Decoding of bounded values with parsers based on attoparsec
module Attoget.AttoDecode (word8Rest, word16BERest) where

import qualified Data.ByteString                 as S
-- import qualified Data.ByteString.Unsafe          as S
import qualified Data.ByteString.Internal        as S
-- import qualified Data.ByteString.Lazy            as L
-- import qualified Data.ByteString.Lazy.Internal   as L
import           Data.ByteString.Char8 () 

import           Data.Attoparsec.ByteString     
import qualified Data.Attoparsec.Internal.Types as T
import           Data.Attoparsec.Internal.Types
                   hiding (Parser, Input, Added, Failure, Success)

import           Control.Applicative

import           Foreign

import           Prelude hiding (getChar, take, takeWhile)


------------------------------------------------------------------------------
-- Low-level parsers from Data.Attoparsec.ByteString.Internal
------------------------------------------------------------------------------

type Input       = T.Input S.ByteString
type Added       = T.Added S.ByteString
type Failure r   = T.Failure S.ByteString r
type Success a r = T.Success S.ByteString a r

-- | If at least @n@ bytes of input are available, return the current
-- input, otherwise fail.
ensure :: Int -> Parser S.ByteString
ensure !n = T.Parser $ \i0 a0 m0 kf ks ->
    if S.length (unI i0) >= n
    then ks i0 a0 m0 (unI i0)
    else T.runParser (demandInput >> ensure n) i0 a0 m0 kf ks

-- | Ask for input.  If we receive any, pass it to a success
-- continuation, otherwise to a failure continuation.
prompt :: Input -> Added -> More
       -> (Input -> Added -> More -> Result r)
       -> (Input -> Added -> More -> Result r)
       -> Result r
prompt i0 a0 _m0 kf ks = Partial $ \s ->
    if S.null s
    then kf i0 a0 Complete
    else ks (I (unI i0 <> s)) (A (unA a0 <> s)) Incomplete

-- | Immediately demand more input via a 'Partial' continuation
-- result.
demandInput :: Parser ()
demandInput = T.Parser $ \i0 a0 m0 kf ks ->
    if m0 == Complete
    then kf i0 a0 m0 ["demandInput"] "not enough bytes"
    else let kf' i a m = kf i a m ["demandInput"] "not enough bytes"
             ks' i a m = ks i a m ()
         in prompt i0 a0 m0 kf' ks'

-- | This parser always succeeds.  It returns 'True' if any input is
-- available either immediately or on demand, and 'False' if the end
-- of all input has been reached.
wantInput :: Parser Bool
wantInput = T.Parser $ \i0 a0 m0 _kf ks ->
  case () of
    _ | not (S.null (unI i0)) -> ks i0 a0 m0 True
      | m0 == Complete  -> ks i0 a0 m0 False
      | otherwise       -> let kf' i a m = ks i a m False
                               ks' i a m = ks i a m True
                           in prompt i0 a0 m0 kf' ks'

get :: Parser S.ByteString
get  = T.Parser $ \i0 a0 m0 _kf ks -> ks i0 a0 m0 (unI i0)

put :: S.ByteString -> Parser ()
put s = T.Parser $ \_i0 a0 m0 _kf ks -> ks (I s) a0 m0 ()


------------------------------------------------------------------------------
-- Fixed-Size Decodings
------------------------------------------------------------------------------

data FD a = FD { fdSize :: {-# UNPACK #-} !Int,  fdIO :: (Ptr Word8 -> IO a) }

instance Functor FD where
  fmap f (FD s io) = FD s (fmap (fmap f) io)

instance Applicative FD where
  pure x  = FD 0 (const $ return x)

  FD fs fio <*> FD xs xio = 
      FD (fs + xs) (\ip -> fio ip <*> xio (ip `plusPtr` fs))

word8FD :: FD Word8
word8FD = FD 1 peek

word16BE :: FD Word16
word16BE = 
    (\w1 w2 -> fromIntegral w1 `shiftL` 8 .|. fromIntegral w2) 
    <$> word8FD <*> word8FD


{-# INLINE evalFD #-}
evalFD :: FD a -> S.ByteString -> a
evalFD fd (S.PS fp off _) = 
    S.inlinePerformIO $ withForeignPtr fp $ \p -> fdIO fd (p `plusPtr` off)

-- | Decode with a fixed decoding.
decodeWithFD :: FD a -> Parser a
decodeWithFD fd = evalFD fd <$> take (fdSize fd)

-- | Decode the rest of the input with the given fixed decoding. There may be
-- left-over input smaller than the fixed decoding.
{-# INLINE decodeRestWithFD #-}
decodeRestWithFD :: forall a. FD a -> Parser [a]
decodeRestWithFD fd = 
    decodeChunk id
  where
    decodeChunk xsC0 = 
        (ensure (fdSize fd) >>= decode) <|> return (xsC0 [])
      where
        decode (S.PS fpbuf o l) =
            S.inlinePerformIO $ go xsC0 ip0 <* touchForeignPtr fpbuf
          where
            pbuf = unsafeForeignPtrToPtr fpbuf
            ip0  = pbuf `plusPtr` o
            ipe  = ip0 `plusPtr` l

            go :: ([a] -> [a]) -> Ptr Word8 -> IO (Parser [a])
            go xsC !ip 
              | ip' <= ipe = do
                    x <- fdIO fd ip
                    go (xsC . (x :)) ip'
              | otherwise = return $ put bs' >> decodeChunk xsC
              where
                ip' = ip `plusPtr` fdSize fd
                bs' = S.PS fpbuf (ip `minusPtr` pbuf) (ipe `minusPtr` ip)

word8Rest :: Parser [Word8]
word8Rest = decodeRestWithFD word8FD

word16BERest :: Parser [Word16]
word16BERest = decodeRestWithFD word16BE
{-

unpackWithFD :: FD a -> L.ByteString -> Maybe [a]
unpackWithFD fd lbs = case runParser (decodeListWithFD fd >> eof) lbs of
    Left _  -> Nothing
    Right x -> Just x
-}

{-



-- Plan for a faster binary serialization format:
--
-- 1. Use chunked encoding for bounded types. Decoding is then factored into
--    extracting a lazy bytestring from the chunk lengths and decoding this
--    lazy bytestring efficiently.
--
-- 2. There are two different efficiency problems:
--      1. Ensure that one pays for backtracking only if one uses it.
--      2. Ensure that bounded types are parsed efficiently; i.e., if there
--         is enough input remaining straight-line IO code should be used.
--
-- 3. Fine-tuning will require that special attentation is paid to strictness
--    issues. They should however not be too difficult.
--    

{- DRAFT: bounded decoding.
 
-- failing, bounded decode, fast/slow
data BoundedDecode a = BoundedDecode 
       Int 
       (Parser a)
      -- (Ptr Word8 -> IO (Maybe (a, Int)))

int16LE :: BoundedDecode Int32
int16LE = BoundedDecode 4 
    ((\a b -> (fromIntegral a `shiftL` 8) .|. fromIntegral b) <$> word8 <*> word8)
    -- (\ip -> do x <- peek (castPtr ip); return (Just (x, 2)))

{-
decode :: BoundedDecode a -> Parser a
decode (BoundedDecode bound p io) = 
    Parser $ parse
  where
    parse k = ParseStep step
      where
        step bs@(S.PS fpbuf off len)
          | len < bound = runParseStep (unParser p k) bs
          | otherwise   = case S.inlinePerformIO (io ip) of
              Nothing -> Fail None bs
              Just (x, i) -> 
                let bs' = S.PS fpbuf (off + i) (len - i)
                in  bs' `seq` runParseStep (k x) bs'
          where
            pbuf = unsafeForeignPtrToPtr fpbuf
            ip   = pbuf `plusPtr` off
-}

instance Functor BoundedDecode where
    fmap f (BoundedDecode bound p) = 
      BoundedDecode bound (fmap f p) -- (fmap (fmap (fmap (first f))) io)

instance Applicative BoundedDecode where
    pure x = BoundedDecode 0 (return x) -- (const (return (Just (x,0))))
    (BoundedDecode fBound fP) <*> (BoundedDecode xBound xP) =
        BoundedDecode (fBound + xBound) (fP <*> xP) {- $ \ip -> do
            fRes <- fIO ip
            case fRes of
                Nothing -> return Nothing
                Just (f, fN) -> do
                    xRes <- xIO (ip `plusPtr` fN)
                    case xRes of
                        Nothing      -> return Nothing
                        Just (x, xN) -> return (Just (f x, fN + xN)) -}


data TaggedDecode t a = TaggedDecode Int (t -> Maybe (Parser a))

instance Functor (TaggedDecode t) where
    fmap f (TaggedDecode bound p) = TaggedDecode bound (fmap (fmap f) . p)

instance Applicative (TaggedDecode t) where
    pure x = TaggedDecode 0 (pure $ pure $ pure x)

    (TaggedDecode fBound fP) <*> (TaggedDecode xBound xP) =
        TaggedDecode (fBound + xBound) 
                     (\tag -> liftA2 (<*>) (fP tag) (xP tag))

instance Alternative (TaggedDecode t) where
    empty = TaggedDecode 0 (const Nothing)
    (TaggedDecode xBound xP) <|> (TaggedDecode yBound yP) =
       TaggedDecode (max xBound yBound) (\tag -> xP tag <|> yP tag)

withTag :: Eq t => t -> BoundedDecode a -> TaggedDecode t a
withTag tag (BoundedDecode xBound xP) = 
    TaggedDecode xBound (\tag' -> if tag == tag' then Just xP else Nothing)

taggedToBounded :: BoundedDecode t -> TaggedDecode t a -> BoundedDecode a
taggedToBounded (BoundedDecode tagBound tagP) (TaggedDecode xBound xP) =
    BoundedDecode (tagBound + xBound) (tagP >>= (fromMaybe (fail "no tag") . xP))

-}

{-
instance Alternative

ifD :: BoundedDecode tag -> (tag, BoundedDecode a) -> 

tag1 payload1 | tag2 payload2
-}

--
-- unfailing, fixed decode, fast/slow
-- unfailing, bounded decode
-- failing, fixed decode
-- failing, bounded decode



-- Fixed size, possibly failing reads.
data ReadF a = ReadF  Int  -- size
                     (Ptr Word8 -> IO (Either String a))

instance Functor ReadF where
    fmap f (ReadF s io) = ReadF s (fmap (fmap f) . io)

instance Applicative ReadF where
    pure x  = ReadF 0 (const $ return $ Right x)
    ReadF fs fio <*> ReadF xs xio = 
        ReadF (fs + xs) (\p -> do 
          fres <- fio p
          case fres of
            Right f -> do 
              xres <- xio (p `plusPtr` fs)
              case xres of
                Right x -> return $ Right $ f x
                Left m  -> return $ Left m
            Left m  -> return $ Left m)


word8 :: ReadF Word8
word8 = ReadF 1 $ \p -> do x <- peek p
                           return $ Right x
                 


readF :: ReadF a -> Reader a
readF (ReadF size io) = 
    Reader step
  where
    step k (BR ifp ip ipe)
      | size <= inRemaining = do
          res <- io ip
          case res of
              Left err -> return $ Fail ip err
              Right x  -> do
                let !br' = (BR ifp (ip `plusPtr` size) ipe)
                k x br'
      | otherwise = do
          tfp <- S.mallocByteString size
          let tp0  = unsafeForeignPtrToPtr tfp
              tpe0 = tp0 `plusPtr` size
          copyBytes tp0 ip inRemaining
          let more tp (BR ifp' ip' ipe') 
                | inRemaining' <= 0             = return $ Fail ip "out-of-space"
                | inRemaining' <  tempRemaining = do
                    copyBytes tp ip' inRemaining'
                    return $ Partial $ more (tp `plusPtr` inRemaining')
                | otherwise = do
                    copyBytes tp ip' tempRemaining
                    res <- io tp
                    touchForeignPtr tfp
                    case res of
                        Left err -> return $ Fail ip err
                        Right x  -> do
                          let !br' = (BR ifp' (ip' `plusPtr` tempRemaining) ipe')
                          k x br'
                where
                  inRemaining'  = ipe' `minusPtr` ip'
                  tempRemaining = tpe0 `minusPtr` tp

          return $ Partial $ more (tp0 `plusPtr` inRemaining)
      where
        inRemaining = ipe `minusPtr` ip

        
-}
