{-# LANGUAGE Rank2Types, CPP, BangPatterns #-}
-- | Attoparsec base types without additional cost for non-backtracking
-- parsers.
module Attoget where

import qualified Data.ByteString                 as S
import qualified Data.ByteString.Unsafe          as S
-- import qualified Data.ByteString.Internal        as S
import qualified Data.ByteString.Lazy            as L
import qualified Data.ByteString.Lazy.Internal   as L
import           Data.ByteString.Char8 () 

import           Control.Applicative
import           Control.Monad
import           Criterion.Main (defaultMain, nf, bench, bgroup)

import           Foreign

-- Imports benchmarking purposes
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Binary.Get                 as B

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

-- | Signal of the parser to its driver.
data Signal a = 
         Result  a S.ByteString
         -- ^ Result together with remaining input.
       | Fail [String] S.ByteString 
         -- ^ Failure, stack of error messages, and remaining input.
         -- Non-strict in the second argument to avoid redundant work in case
         -- of nested 'try's.
       | Partial (ParseStep a)
         -- ^ More input is required.

-- | A parsing step. This is already a fully, fledged parsing monad. The
-- only difference to the 'Parser' type is that it does not use CPS for
-- the bind operator.
newtype ParseStep r = ParseStep 
          { runParseStep :: S.ByteString -> Signal r }

-- | A CPS version of 'ParseStep'.
newtype Parser a = Parser 
          { unParser :: forall r. (a -> ParseStep r) -> ParseStep r }


-- ParseStep operations
-----------------------

-- | Parsec style alternative for 'ParseStep'.
{-# INLINE plus_ps #-}
plus_ps :: ParseStep a -> ParseStep a -> ParseStep a
plus_ps ps1 ps2 = 
    ParseStep $ runPS 0 ps1
  where
    runPS len0 ps bs = case runParseStep ps bs of
        Fail _errs rest 
          | S.length rest == len -> runParseStep ps2 rest
        Partial next             -> Partial (ParseStep $ runPS len next)
        signal                   -> signal
      where
        !len = len0 + S.length bs

-- | Parsec's 'try' operator.
try_ps :: ParseStep a -> ParseStep a
try_ps ps = 
    ParseStep $ run id (runParseStep ps)
  where
    run lbsC step bs = case step bs of
        signal@(Result _ _) -> signal
        Partial next        -> Partial $
            ParseStep $ run (lbsC . L.chunk bs) (runParseStep next)
        -- The lazyness of 'Fail' in its second argument is crucial here.
        -- Otherwise, nested 'try's would perform redundant work, as only the
        -- stored input of the outermost 'try' will be used for the next
        -- parser.
        Fail errs _ -> 
           Fail ("try" : errs) (S.concat $ L.toChunks $ lbsC L.empty)


-- Parser operations
--------------------

-- | Parsec style alternative for 'Parser'.
{-# INLINE plus #-}
plus :: Parser a -> Parser a -> Parser a
plus p1 p2 = 
    Parser $ runP
  where
    runP k = 
        runPS 0 (unParser p1 return)
      where
        runPS !len0 ps = ParseStep $ \bs -> 
            case runParseStep ps bs of
                Result x rest  -> runParseStep (k x) rest
                Partial next   -> 
                  let !len = len0 + S.length bs in Partial (runPS len next)
                Fail errs rest 
                  | S.length rest == len0 + S.length bs ->
                      runParseStep (unParser p2 k) rest
                  | otherwise -> Fail errs rest

-- | Parsec's 'try' operator for 'Parser'.
try :: Parser a -> Parser a
try p = 
    Parser $ runP
  where
    runP k = ParseStep $ runPS id (runParseStep $ unParser p return)
      where
        runPS lbsC step bs = case step bs of
          Result x rest -> runParseStep (k x) rest
          Partial next  -> Partial $
              ParseStep $ runPS (lbsC . L.chunk bs) (runParseStep next)
          Fail errs _ -> 
              Fail ("try" : errs) (S.concat $ L.toChunks $ lbsC L.empty)


-- Instances
------------

instance Functor Signal where
  {-# INLINE fmap #-}
  fmap f (Result x inp)  = Result (f x) inp
  fmap f (Partial next)  = Partial (fmap f next)
  fmap _ (Fail errs inp) = Fail errs inp


instance Functor ParseStep where
  {-# INLINE fmap #-}
  fmap f = ParseStep . fmap (fmap f) . runParseStep

instance Applicative ParseStep where
  {-# INLINE pure #-}
  pure  = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Alternative ParseStep where
  {-# INLINE empty #-}
  empty = fail "alternative: empty"

  {-# INLINE (<|>) #-}
  (<|>) = plus_ps

instance Monad ParseStep where
  {-# INLINE return #-}
  return x  = ParseStep $ Result x

  {-# INLINE (>>=) #-}
  m >>= f   = ParseStep $ \bs -> 
                  case runParseStep m bs of
                      Result x rest  -> runParseStep (f x) rest
                      Partial next   -> Partial (next >>= f)
                      Fail errs rest -> Fail errs rest

  {-# INLINE (>>) #-}
  m >> m'   = ParseStep $ \bs -> 
                  case runParseStep m bs of
                      Result _ rest  -> runParseStep m' rest
                      Partial next   -> Partial (next >> m')
                      Fail errs rest -> Fail errs rest

  {-# INLINE fail #-}
  fail msg = ParseStep $ Fail [msg]

instance MonadPlus ParseStep where
  {-# INLINE mzero #-}
  mzero = empty
  {-# INLINE mplus #-}
  mplus = (<|>)



-- Parser
---------

instance Functor Parser where
  {-# INLINE fmap #-}
  fmap f p = Parser $ \k -> unParser p (k . f)

instance Applicative Parser where
  {-# INLINE pure #-}
  pure x  = Parser $ \k -> k x
  {-# INLINE (<*>) #-}
  f <*> x = Parser $ \k -> unParser f (\f' -> unParser x (\x' -> k (f' x')))
  {-# INLINE (<*) #-}
  x <* y  = Parser $ \k -> unParser x (\x' -> unParser y (\_ -> k x'))
  {-# INLINE (*>) #-}
  x *> y  = Parser $ \k -> unParser x (\_ -> unParser y k)

instance Alternative Parser where
  {-# INLINE empty #-}
  empty = fail "alternative: empty"
  {-# INLINE (<|>) #-}
  (<|>) = plus

instance Monad Parser where
  {-# INLINE return #-}
  return   = pure
  {-# INLINE (>>=) #-}
  x >>= f  = Parser $ \k -> unParser x (\x' -> unParser (f x') k)
  {-# INLINE (>>) #-}
  (>>) = (*>)
  {-# INLINE fail #-}
  fail err = Parser $ \_ -> fail err

-- Concrete Parsers
-------------------

-- | Mark an unexpected end of input.
{-# INLINE unexpectedEOI #-}
unexpectedEOI :: String -> S.ByteString -> Signal a
unexpectedEOI loc = Fail [loc ++ ": unexpected end of input"]

-- | Parse an unsigned byte.
word8 :: Parser Word8
word8 = Parser $ parse
  where
    parse k = ParseStep step
      where
        step bs
          | S.null bs = Partial $ ParseStep $ \bs' ->
              if S.null bs' then unexpectedEOI "word8" bs' else getWord8 bs'
        step bs = getWord8 bs

        getWord8 bs = runParseStep (k (S.unsafeHead bs)) (S.unsafeTail bs)

-- | Parses the whole remaining input as a lazy bytestring.
lazyByteString :: Parser L.ByteString
lazyByteString = Parser $ parse
  where
    parse k = ParseStep $ step0
      where
        step0 bs = Partial $ ParseStep $ step1 (L.chunk bs)
        step1 lbsC bs
          | S.null bs = runParseStep (k (lbsC L.empty)) bs
          | otherwise = Partial $ ParseStep $ step1 (lbsC . L.chunk bs)

-- | Run a parser. 
runParser :: Parser a -> L.ByteString -> (Either [String] a, L.ByteString)
runParser p = 
    feed (unParser p return)
  where
    feed ps lbs0 = case runParseStep ps bs' of
        Partial next -> feed next lbs' 
        Result x bs  -> (Right x,   L.chunk bs lbs')
        Fail errs bs -> (Left errs, L.chunk bs lbs')
      where
        (bs',lbs') = case lbs0 of
          L.Empty        -> (S.empty, L.empty)
          L.Chunk bs lbs -> (bs, lbs)

------------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------------

-- | Size of test data.
nRepl :: Int
nRepl = 10000

{-# NOINLINE word8Data #-}
word8Data :: L.ByteString
word8Data = L.pack $ take nRepl $ cycle [0..]

-- | Data to test the speed of parsing the naive binary list serialization 
-- format: cons tagged with 1, nil tagged with 0.
{-# NOINLINE binaryData #-}
binaryData :: L.ByteString
binaryData = L.pack $ 
    (++ [0]) $ concatMap (\x -> [1,x]) $ take (nRepl `div` 2) $ cycle [0..]

parseManyWord8s :: Parser [Word8]
parseManyWord8s = many word8 

parseNWord8s :: Parser [Word8]
parseNWord8s = sequence $ replicate nRepl word8

parseBinaryWord8s :: Parser [Word8]
parseBinaryWord8s = do
    tag <- word8
    case tag of 
      0 -> return []
      1 -> (:) <$> word8 <*> parseBinaryWord8s
      _ -> fail $ "parseBinaryWord8s: unknown tag " ++ show tag

manyWord8sViaUnpack :: Parser [Word8]
manyWord8sViaUnpack = L.unpack <$> lazyByteString

-- Attoparsec
-------------

attoManyWord8s :: A.Parser [Word8]
attoManyWord8s = many A.anyWord8 

attoNWord8s :: A.Parser [Word8]
attoNWord8s = sequence $ replicate nRepl A.anyWord8

attoBinaryWord8s :: A.Parser [Word8]
attoBinaryWord8s = do
    tag <- A.anyWord8
    case tag of 
      0 -> return []
      1 -> (:) <$> A.anyWord8 <*> attoBinaryWord8s
      _ -> fail $ "attoBinaryWord8s: unknown tag " ++ show tag


-- Binary Get
-------------

-- Does not work in the 'Get' monad, as it does not support error recovery.
-- getManyWord8s :: B.Get [Word8]
-- getManyWord8s = many B.getWord8 

getNWord8s :: B.Get [Word8]
getNWord8s = sequence $ replicate nRepl B.getWord8

getBinaryWord8s :: B.Get [Word8]
getBinaryWord8s = do
    tag <- B.getWord8
    case tag of 
      0 -> return []
      1 -> (:) <$> B.getWord8 <*> getBinaryWord8s
      _ -> fail $ "getBinaryWord8s: unknown tag " ++ show tag


-- Benchmarking main
--------------------

main :: IO ()
main = defaultMain
  [ bgroup "bytestring"
      [ bench "unpack" $ nf L.unpack word8Data ]

  , bgroup "binaryget"
      [ benchGet "getNWord8s"         getNWord8s word8Data
      , benchGet "getBinaryWord8s"    getBinaryWord8s binaryData
      ]

  , bgroup "attoget"
      [ benchParse "parseManyWord8s"     parseManyWord8s word8Data
      , benchParse "parseNWord8s"        parseNWord8s word8Data
      , benchParse "parseBinaryWord8s"   parseBinaryWord8s binaryData
      , benchParse "manyWord8sViaUnpack" manyWord8sViaUnpack word8Data 
      ]

  , bgroup "attoparsec"
      [ benchAtto "attoManyWord8s"      attoManyWord8s word8Data
      , benchAtto "attoNWord8s"         attoNWord8s word8Data
      , benchAtto "attoBinaryWord8s"    attoBinaryWord8s binaryData
      ]
  ]
  where
    benchParse name p inp = bench name $ nf (fst . runParser p) inp

    benchAtto name p inp = bench name $ nf (A.eitherResult . A.parse p) inp

    benchGet name p inp = bench name $ nf (B.runGet p) inp

{-
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
        step (unParser p1 Result)
      where
        step ps bs = case sig of
            Result x bs'      -> k x bs'
            Partial None next -> Partial None (step next)
            Partial Some next -> Partial Some (next >>= k)
            Fail None bs'     -> unParser p2 k bs'
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
-}
