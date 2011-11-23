{-# LANGUAGE Rank2Types, CPP, BangPatterns #-}
-- | Attoparsec base types without additional cost for non-backtracking
-- parsers.
module Benchmarks where

import qualified Data.ByteString.Lazy            as L
import           Data.ByteString.Char8 () 

import           Control.Applicative
import           Criterion.Main (defaultMain, nf, bench, bgroup)

import           Foreign

import           Attoget.Hybrid

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

