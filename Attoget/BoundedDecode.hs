{-# LANGUAGE BangPatterns #-}
-- | Decoding of bounded values.
module Attoget.BoundedDecode where

import qualified Data.ByteString                 as S
import qualified Data.ByteString.Unsafe          as S
import qualified Data.ByteString.Internal        as S
import qualified Data.ByteString.Lazy            as L
import qualified Data.ByteString.Lazy.Internal   as L
import           Data.ByteString.Char8 () 

import           Control.Applicative

import           Foreign

import           Attoget.Hybrid



data FD a = FD { fdSize :: {-# UNPACK #-} !Int,  fdIO :: (Ptr Word8 -> IO a) }

instance Functor FD where
  fmap f (FD s io) = FD s (fmap (fmap f) io)

instance Applicative FD where
  pure x  = FD 0 (const $ return x)

  FD fs fio <*> FD xs xio = 
      FD (fs + xs) (\ip -> fio ip <*> xio (ip `plusPtr` fs))

{-
-- | Try to ensure that at least 'n' bytes are available and
-- return the currently available chunks.
ensureLazy :: Int -> Parser L.ByteString
ensureLazy n0 = Parser parse 
  where
    parse k = ParseStep $ step id n0
      where
        step lbsC n bs
          | S.null bs        = runParseStep (k (lbsC L.empty)) S.empty
          | n <= S.length bs = runParseStep (k (lbsC L.empty)) bs
-}
-- | Smart constructor for a partial parse.
partial :: (S.ByteString -> Signal a) -> Signal a
partial = Partial . ParseStep

-- | Strictification of a lazy bytestring.
toStrict :: L.ByteString -> S.ByteString
toStrict L.Empty             = S.empty
toStrict (L.Chunk c L.Empty) = c
toStrict cs0 = S.unsafeCreate totalLen $ \ptr -> go cs0 ptr
  where
    totalLen = L.foldlChunks (\a c -> a + S.length c) 0 cs0

    go L.Empty                        !_       = return ()
    go (L.Chunk (S.PS fp off len) cs) !destptr =
      withForeignPtr fp $ \p -> do
        copyBytes destptr (p `plusPtr` off) len
        go cs (destptr `plusPtr` len)

-- | Take a bytestring of the given length. Fails if not enough bytes
-- are available.
takeByteString :: Int -> Parser S.ByteString
takeByteString n0 
    | n0 <= 0   = return S.empty
    | otherwise = Parser parse
  where
    parse k = ParseStep $ step0 
      where
        step0 bs
          | n0 <= len = -- common case. progress ensured, as n0 > 0
              runParseStep (k (S.unsafeTake n0 bs)) (S.unsafeDrop n0 bs)
          | otherwise = partial $ step1 (L.chunk bs) (n0 - len)
          where
            len = S.length bs

        -- an empty bytestring means EOI
        step1 lbsC !n bs
          | S.null bs = Fail "takeByteString: unexpected EOI" bs
          | len < n   = partial $ step1 lbsC' (n - len)
          | otherwise = case L.splitAt (fromIntegral n0) $ lbsC' L.empty of
              (pre, post) -> runParseStep (k (toStrict pre)) (toStrict post)
          where
            lbsC' = lbsC . L.Chunk bs
            len   = S.length bs


-- | Decode with a fixed decoding.
decodeWithFD :: FD a -> Parser a
decodeWithFD fd =
    Parser $ parse
  where
    s = fdSize fd

    parse k = ParseStep step
      where
        step bs
          | s <= S.length bs = 
              runParseStep (k (evalFD fd bs)) (S.unsafeDrop s bs)
          | otherwise        = 
              runParseStep (unParser (evalFD fd <$> takeByteString s) k) bs

{-
-- | Decode the rest of the input with the given fixed decoding. There may be
-- left-over input smaller than the fixed decoding.
decodeListWithFD :: FD a -> Parser [a]
decodeListWithFD fd =
    Parser $ parse
  where
    s = fdSize fd

    parse k = ParseStep $ step0 id
      where
        step0 bs
          | S.null bs = partial $ step1 id
          | otherwise = step1 id bs

        step1 xsC bs
          | s <= S.length bs = 
              runParseStep (k (evalFD fd bs)) (S.unsafeDrop s bs)
          | otherwise        = 
              runParseStep (unParser (evalFD fd <$> takeByteString s) k) bs
-}

{-
eof :: Parser ()
eof = Parser $ \k -> ParseStep $ 

unpackWithFD :: FD a -> L.ByteString -> Maybe [a]
unpackWithFD fd lbs = case runParser (decodeListWithFD fd >> eof) lbs of
    Left _  -> Nothing
    Right x -> Just x
-}

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