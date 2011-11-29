{-# LANGUAGE Rank2Types, CPP, BangPatterns #-}
-- | Attoparsec base types without additional cost for non-backtracking
-- parsers.
module Main (main) where

import qualified Data.ByteString.Lazy            as L
import           Data.ByteString.Char8 () 

import           Control.Applicative
import           Criterion.Main (defaultMain, nf, bench, bgroup)

import           Foreign

import           Attoget.Hybrid
import           Attoget.AttoDecode 

-- Imports benchmarking purposes
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Binary.Get                 as B


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

-- | Binary data as long as the 'word8Data'. Comparing the decoding time for
-- this data agains unpacking 'word8Data' gives a good estimate of the time we
-- can save for switching to a chunked format.
{-# NOINLINE binaryDataFull #-}
binaryDataFull :: L.ByteString
binaryDataFull = L.pack $ 
    (++ [0]) $ concatMap (\x -> [1,x]) $ take nRepl $ cycle [0..]

parseManyWord8s :: Parser [Word8]
parseManyWord8s = many word8 

parseNWord8s :: Parser [Word8]
parseNWord8s = sequence $ replicate nRepl word8

manyWord8sViaUnpack :: Parser [Word8]
manyWord8sViaUnpack = L.unpack <$> lazyByteString

{-# INLINE genParseBinaryWord8s #-}
genParseBinaryWord8s :: (Parser Word8) -> Parser [Word8]
genParseBinaryWord8s w8 = 
    go
  where
    go = do
        tag <- w8
        case tag of 
          0 -> return []
          1 -> (:) <$> w8 <*> go
          _ -> fail $ "parseBinaryWord8s: unknown tag " ++ show tag

parseBinaryWord8s :: Parser [Word8]
parseBinaryWord8s = genParseBinaryWord8s word8

parseBinaryWord8sNoInline :: Parser [Word8]
parseBinaryWord8sNoInline = genParseBinaryWord8s parseWord8_noinline

{-# NOINLINE parseWord8_noinline #-}
parseWord8_noinline :: Parser Word8
parseWord8_noinline = word8


-- Attoparsec
-------------

attoManyWord8s :: A.Parser [Word8]
attoManyWord8s = many A.anyWord8 

attoNWord8s :: A.Parser [Word8]
attoNWord8s = sequence $ replicate nRepl A.anyWord8

{-# INLINE genAttoBinaryWord8s #-}
genAttoBinaryWord8s :: (A.Parser Word8) -> A.Parser [Word8]
genAttoBinaryWord8s w8 = do
    go
  where
    go = do
        tag <- w8
        case tag of 
          0 -> return []
          1 -> (:) <$> w8 <*> go
          _ -> fail $ "parseBinaryWord8s: unknown tag " ++ show tag

attoBinaryWord8s :: A.Parser [Word8]
attoBinaryWord8s = genAttoBinaryWord8s A.anyWord8

attoBinaryWord8sNoInline :: A.Parser [Word8]
attoBinaryWord8sNoInline = genAttoBinaryWord8s attoWord8_noinline

{-# NOINLINE attoWord8_noinline #-}
attoWord8_noinline :: A.Parser Word8
attoWord8_noinline = A.anyWord8


-- Binary Get
-------------

-- Does not work in the 'Get' monad, as it does not support error recovery.
-- getManyWord8s :: B.Get [Word8]
-- getManyWord8s = many B.getWord8 

getNWord8s :: B.Get [Word8]
getNWord8s = sequence $ replicate nRepl B.getWord8

{-# INLINE genGetBinaryWord8s #-}
genGetBinaryWord8s :: (B.Get Word8) -> B.Get [Word8]
genGetBinaryWord8s w8 = do
    go
  where
    go = do
        tag <- w8
        case tag of 
          0 -> return []
          1 -> (:) <$> w8 <*> go
          _ -> fail $ "parseBinaryWord8s: unknown tag " ++ show tag

getBinaryWord8s :: B.Get [Word8]
getBinaryWord8s = genGetBinaryWord8s B.getWord8

getBinaryWord8sNoInline :: B.Get [Word8]
getBinaryWord8sNoInline = genGetBinaryWord8s getWord8_noinline

{-# NOINLINE getWord8_noinline #-}
getWord8_noinline :: B.Get Word8
getWord8_noinline = B.getWord8

-- Benchmarking main
--------------------

main :: IO ()
main = defaultMain
  [ -- Compare how fast we can unpack the input to a list of 'Word8's. This is
    -- not a realistic benchmarks, but measures very well the abstraction
    -- overhead of different parser implementations.
    bgroup "manyWord8s"
      [ bench "unpack" $ nf L.unpack word8Data 
      , benchParse' "attoget (via-unpack)"     manyWord8sViaUnpack word8Data 
      , benchParse     parseManyWord8s word8Data
      , benchAtto      attoManyWord8s word8Data
      , benchAtto' "attodecode ->" (decodeRestWithFD      word8FD)   word8Data
      , benchAtto' "attodecode ->'" (decodeRestWithFD'    word8FD)   word8Data
      , benchAtto' "attodecode <-" (decodeRestWithFD_bw   word8FD)   word8Data
      , benchAtto' "attodecode <-'" (decodeRestWithFD_bw' word8FD)   word8Data
      ]

    -- compare how fast we can unpack as many Word16 as possible from the
    -- current input.
  , bgroup "manyWord16BEs"
      [ benchAtto' "attodecode ->" (decodeRestWithFD      word16BE) word8Data 
      , benchAtto' "attodecode ->'" (decodeRestWithFD'    word16BE) word8Data 
      , benchAtto' "attodecode <-" (decodeRestWithFD_bw   word16BE) word8Data
      , benchAtto' "attodecode <-'" (decodeRestWithFD_bw' word16BE) word8Data
      ]

    -- Here we measure the time for parsing the tagged list format used by
    -- Data.Binary for 'nRepl' words. Compare this to the time for just
    -- unpacking 'nRepl' words to get a bound on the maximal speed improvement
    -- for using a chunked encoding.
  , bgroup "binaryWord8s (full length)"
      [ benchGet           getBinaryWord8s   binaryDataFull
      , benchParse         parseBinaryWord8s binaryDataFull
      , benchAtto          attoBinaryWord8s  binaryDataFull
      , benchParseViaAtto  parseBinaryWord8s binaryDataFull
      ] 

    -- Compare to the time for parsing the binaryDataFull, which encodes twice
    -- the number of bytes to estimate the scaling behaviour of the different
    -- parsing alternatives.
  , bgroup "binaryWord8s"
      [ benchGet           getBinaryWord8s   binaryData
      , benchParse         parseBinaryWord8s binaryData
      , benchAtto          attoBinaryWord8s  binaryData
      , benchParseViaAtto  parseBinaryWord8s binaryData
      ] 

    -- Compare to the speed of 'binaryWord8s' to see the cost of fully
    -- abstract calls to the '>>=' operator.
  , bgroup "binaryWord8s NOINLINE"
      [ benchGet           getBinaryWord8sNoInline   binaryData
      , benchParse         parseBinaryWord8sNoInline binaryData
      , benchAtto          attoBinaryWord8sNoInline  binaryData
      , benchParseViaAtto  parseBinaryWord8sNoInline binaryData
      ] 

    -- Check the speed of different parser implementations in the context of
    -- parsing a fixed amount of bytes using a very declarative formulation.
  , bgroup "replicateNWord8s"
      [ benchGet     getNWord8s word8Data
      , benchParse   parseNWord8s word8Data
      , benchAtto    attoNWord8s word8Data
      ]
  ]
  where
    benchParse             = benchParse' "attoget"
    benchParse' name p inp = bench name $ nf (fst . runParser p) inp

    benchParseViaAtto p    = benchAtto' "attoparsec-get" (toAttoparsec p)

    benchAtto              = benchAtto' "attoparsec"
    benchAtto' name p inp  = bench name $ nf (A.eitherResult . A.parse p) inp

    benchGet p inp         = bench "binaryget" $ nf (B.runGet p) inp

