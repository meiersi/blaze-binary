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

import qualified Data.Attoparsec.ByteString     as A
import qualified Data.Attoparsec.Internal.Types as AT

import           Foreign


------------------------------------------------------------------------------
-- Parse signals
------------------------------------------------------------------------------

-- | Signal of the parser to its driver.
data Signal a = 
         Result  a S.ByteString
         -- ^ Result together with remaining input.
       | Fail String S.ByteString 
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


-- ParseStep operations
-----------------------

-- | Parsec style alternative for 'ParseStep'.
{-# INLINE plus_ps #-}
plus_ps :: ParseStep a -> ParseStep a -> ParseStep a
plus_ps ps1 ps2 = 
    ParseStep $ runPS 0 ps1
  where
    runPS len0 ps bs = case runParseStep ps bs of
        Fail _err rest 
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
        Fail err _ -> 
           Fail err (S.concat $ L.toChunks $ lbsC L.empty)


-- Instances
------------

instance Functor Signal where
  {-# INLINE fmap #-}
  fmap f (Result x inp)  = Result (f x) inp
  fmap f (Partial next)  = Partial (fmap f next)
  fmap _ (Fail err inp) = Fail err inp


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
                      Fail err rest -> Fail err rest

  {-# INLINE (>>) #-}
  m >> m'   = ParseStep $ \bs -> 
                  case runParseStep m bs of
                      Result _ rest  -> runParseStep m' rest
                      Partial next   -> Partial (next >> m')
                      Fail err rest -> Fail err rest

  {-# INLINE fail #-}
  fail msg = ParseStep $ Fail msg

instance MonadPlus ParseStep where
  {-# INLINE mzero #-}
  mzero = empty
  {-# INLINE mplus #-}
  mplus = (<|>)


------------------------------------------------------------------------------
-- CPS version of ParseStep
------------------------------------------------------------------------------


-- | A CPS version of 'ParseStep'.
newtype Parser a = Parser 
          { unParser :: forall r. (a -> ParseStep r) -> ParseStep r }


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
                Fail err rest 
                  | S.length rest == len0 + S.length bs ->
                      runParseStep (unParser p2 k) rest
                  | otherwise -> Fail err rest

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
          Fail err _ -> 
              Fail err (S.concat $ L.toChunks $ lbsC L.empty)



-- Instances
------------

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
unexpectedEOI loc = Fail $ loc ++ ": unexpected end of input"

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
runParser :: Parser a -> L.ByteString -> (Either String a, L.ByteString)
runParser p = 
    feed (unParser p return)
  where
    feed ps lbs0 = case runParseStep ps bs' of
        Partial next -> feed next lbs' 
        Result x bs  -> (Right x,   L.chunk bs lbs')
        Fail err bs -> (Left err, L.chunk bs lbs')
      where
        (bs',lbs') = case lbs0 of
          L.Empty        -> (S.empty, L.empty)
          L.Chunk bs lbs -> (bs, lbs)


toAttoparsec :: Parser a -> A.Parser a
toAttoparsec p = 
    feed (unParser p return)
  where
    feed ps = do
        bs <- get
        case runParseStep ps bs of
            Partial next -> do feed next
            Result x bs' -> do put bs'
                               return x
            Fail err bs' -> do put bs'
                               fail err

    -- 'get' and 'put' from the source of Data.Attoparsec.ByteString
    get   = AT.Parser $ \i0  a0 m0 _kf ks -> ks i0 a0 m0 (AT.unI i0)
    put s = AT.Parser $ \_i0 a0 m0 _kf ks -> ks (AT.I s) a0 m0 ()





