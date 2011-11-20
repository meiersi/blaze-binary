{-# LANGUAGE Rank2Types, CPP, BangPatterns #-}
-- | Attoparsec base types without additional cost for non-backtracking
-- parsers.
module Attoget where

import           Data.Maybe (fromMaybe)
import qualified Data.ByteString                 as S
import qualified Data.ByteString.Unsafe          as S
-- import qualified Data.ByteString.Internal        as S
import qualified Data.ByteString.Lazy            as L
import qualified Data.ByteString.Lazy.Internal   as L

import           Control.Applicative
-- import           Control.Arrow

import           Foreign

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

data InputConsumed = None | Some

-- | Unconsumed input is represented as a lazy bytestring continuation to
-- enable O(1) appends.
type LBSC = L.ByteString -> L.ByteString

-- | Signal of the parser to its driver.
data Signal a = 
         Result  a             S.ByteString
         -- ^ Result was computed and some unconsumed input remains.
       | Partial (ParseStep a)
         -- ^ Tells the driver that more input is required.
       | Fail    InputConsumed S.ByteString
         -- ^ Failure with the information whether some input was consumed and
         -- the remaining input.

-- | Type of a parsing step.
newtype ParseStep r = ParseStep { runParseStep :: S.ByteString -> Signal r }

-- | Type of the result continuation
type ResultC r a = a -> ParseStep r

data Parser a = Parser { runParser :: forall r. ResultC r a -> ParseStep r }


instance Functor Signal where
  fmap f (Result x inp) = Result (f x) inp
  fmap f (Partial next) = Partial (fmap f next)
  fmap _ (Fail co inp)  = Fail co inp

instance Functor ParseStep where
  fmap f = ParseStep . fmap (fmap f) . runParseStep

instance Monad ParseStep where
  return x  = ParseStep $ Result x
  m >>= f   = ParseStep $ \bs -> 
                  case runParseStep m bs of
                      Result x rest -> runParseStep (f x) rest
                      Partial next  -> Partial (next >>= f)
                      Fail co rest  -> Fail co rest

  fail _msg = ParseStep $ Fail None

plus_ps :: ParseStep a -> ParseStep a -> ParseStep a
plus_ps ps1 ps2 = ParseStep $ \bs ->
    case runParseStep ps1 bs of
        Partial next         -> Partial (plus_ps next ps2)
        Fail None rest       -> runParseStep ps2 rest
        signal@(Result _ _ ) -> signal
        signal@(Fail Some _) -> signal

try_ps :: ParseStep a -> ParseStep a
try_ps ps = ParseStep $ run id (runParseStep ps)
  where
    run lbsC step bs = case step bs of
        signal@(Result _ _) -> signal
        Partial next        -> Partial $
            ParseStep $ run (lbsC . L.chunk bs) (runParseStep next)
        -- The lazyness of 'Fail' in its second argument is crucial here.
        -- Otherwise, nested 'try's would perform redundant work, as only the
        -- stored input of the outermost 'try' will be used for the next
        -- parser.
        Fail _ _ -> Fail None (S.concat $ L.toChunks $ lbsC L.empty)


-- Parser
---------

instance Functor Parser where
  fmap f p = Parser $ \k -> runParser p (k . f)

instance Applicative Parser where
  pure x  = Parser $ \k -> k x
  f <*> x = Parser $ \k -> runParser f (\f' -> runParser x (\x' -> k (f' x')))

instance Monad Parser where
  return  = pure
  x >>= f = Parser $ \k -> runParser x (\x' -> runParser (f x') k)

plus :: Parser a -> Parser a -> Parser a
plus p1 p2 = 
    Parser $ runP
  where
    runP k = 
        runPS (runParser p1 return)
      where
        runPS ps = ParseStep $ \bs -> 
            case runParseStep ps bs of
                Result x rest  -> runParseStep (k x) rest
                Partial next   -> Partial (runPS next)
                Fail None rest -> runParseStep (runParser p2 k) rest
                Fail Some rest -> Fail Some rest


try :: Parser a -> Parser a
try p = 
    Parser $ runP
  where
    runP k = ParseStep $ runPS id (runParseStep $ runParser p return)
      where
        runPS lbsC step bs = case step bs of
          Result x rest -> runParseStep (k x) rest
          Partial next  -> Partial $
              ParseStep $ runPS (lbsC . L.chunk bs) (runParseStep next)
          Fail _ _ -> Fail None (S.concat $ L.toChunks $ lbsC L.empty)

-- failing, bounded decode, fast/slow
data BoundedDecode a = BoundedDecode 
       Int 
       (Parser a)
      -- (Ptr Word8 -> IO (Maybe (a, Int)))

word8 :: Parser Word8
word8 = Parser $ parse
  where
    parse k = ParseStep step
      where
        step bs
          | S.null bs = Partial $ ParseStep $ \bs' ->
              if S.null bs' then Fail None bs' else getWord8 bs'
        step bs = getWord8 bs

        getWord8 bs = runParseStep (k (S.unsafeHead bs)) (S.unsafeTail bs)

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
          | len < bound = runParseStep (runParser p k) bs
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

-- decodeMaybe :: TaggedDecode
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


{-
feed :: L.ByteString -> ParseStep a -> Signal a
feed L.Empty          step = Partial None step
feed (L.Chunk bs lbs) step = case step bs of
    Result x rest  -> Result x (rest `mappend` lbs)
    sig@(Partial _ next)  
      | L.null lbs -> sig
      | otherwise  -> feed lbs next  -- Code seems to be off by one.
    Fail co rest   -> Fail co (rest `mappend` lbs)


result :: a -> ParseStep a
result x bs = Result x (L.chunk bs L.Empty)
-}

{-
plus :: Parser a -> Parser a -> Parser a
plus p1 p2 = 
    Parser run
  where
    run k = 
        step (runParser p1 Result)
      where
        step ps bs = case sig of
            Result x bs'      -> k x bs'
            Partial None next -> Partial None (step next)
            Partial Some next -> Partial Some (next >>= k)
            Fail None bs'     -> runParser p2 k bs'
            Fail Some bs'     -> Fail Some bs'
          where
            sig = ps bs

-}
{-

plus :: Reader a -> Reader a -> Reader a
plus r1 r2 = 
    Reader $ \k -> step (runReader r1 k)
  where
    run k = 
        step (runReader r1 k)
      where
        step p br = do
          sig <- p br
          case sig of
            Success x driver  -> driver (k x)
            Partial None next -> Partial None (step next)
            Partial Some _    -> return sig
            Fail None driver  -> driver (runReader r2 k)
            Fail Some _       -> return sig


plus :: Reader a -> Reader a -> Reader a
plus r1 r2 = 
    Reader run
  where
    run k = 
        step (runReader r1 (\x br -> Success x [br]))
      where
        -- step :: (BR -> IO (Signal a)) -> IO (Signal a)
        step p br = do
          sig <- p br
          case sig of
            Success x [br']   -> k x br'
            Success x brs     -> feed brs (k x)
            Partial None next -> return (Partial None (step next))
            Partial Some _    -> return sig
            Fail None [br']   -> runReader r2 k br'
            Fail None brs     -> feed brs (runReader r2 k)
            Fail Some _       -> return sig

try :: Reader a -> Reader a
try r = 
    Reader run
  where
    run k = 
        step [] (runReader r (\x br -> Success x [br]))
      where
        step brs p br = do
          sig <- p br
          case sig of
            Success x brs' -> feed brs' (k x)
            Partial _ next -> return (Partial None (step (br:brs) next))
            Fail _ _       -> Fail None (reverse (br:brs))

feed :: [BR] -> (BR -> IO (Signal a)) -> IO (Signal a)
feed []       _    = error "feed: empty"
feed [br]     step = step br
feed (br:brs) step =
    res <- step br
    case res of
      Success brs' x -> Success (brs' ++ brs) x
      Partial _ next -> feed brs next
      Fail c brs'    -> Fail c (brs' ++ brs)

try :: Reader a -> Reader a
try r = 
    Reader $ \step []
  where
    step brs k br@(BR ifp ip ipe) = do
        res <- runReader r (\x (BR _ ip' _) -> Success ip' x) br
        case res of
          Success ip' -> k x (BR ifp ip' ipe)
          Partial c next -> Partial c $ step (br:brs) k next
          Fail _ -> Fail None (reservse brs)

      



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
