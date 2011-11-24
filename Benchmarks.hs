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

-- Imports benchmarking purposes
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Binary.Get                 as B

{- 
Lately, I was thinking about how to improve the speed of the 'binary'
serialization library. There are two main improvement vectors.

1. We can improve the datastructures/abstractions used to encode and decode
   binary data.

2. We can change the serialization format to allow for faster encoding and
   decoding.

Note that the improvements may come at the cost of compatibility with the
existing API. If it warrants significant speed improvments, then that is fine
in my opinion.

In earlier posts, I focused on the lazy bytestring 'Builder' abstraction,
which allows to implement efficient encodings, i.e., functions mapping Haskell
values to sequences of bytes. The blaze-builder library provided an early
prototype of a lazy bytestring builder. Recently, I have completed a
significant rewrite of it, which will be available in the next release of the
bytestring library.

In this post, I provide benchmark results that allow a more informed choice for
the design and implementation of the decoding side of a 'binary' serialization
library. The baseline is provided by the parser implemented in the 'Get' monad
of the current 'binary-0.5.0.2' library, which is called 'binaryget' in the
following benchmarks. This parser is a simple state monad over the input
represented as a lazy bytestring. It does not support failure recovery. An
obvious competition is the bytestring parser provided by the 'attoparsec' libary.
It is implemented using continuation-passing-style (CPS) and supports 
failure recovery and partial parses (enumerator style parsing). 

Note that failure recovery is not required to parse a binary serialization
format. 

Note that we have full control over our binary encoding format.  and therefore
we can avoid

failur recovery is not required to parse a serializad Haskell value.

Common sense
says that su

based parserdoes not support failure recovery



of the situation for 

show a few benchmark results that 

Recently, amy work on a lazy bytestring builder 
.

With my work on the blaze-builder library, which is now integrated in the
bytestring repository


Its primary goal
is to improve encoding (serialization) and decoding (deserialization) speed.
Where possible, it should retain this 

The design goals for a serialization format are obviously maximal speed for
both encoding and decoding. For encoding, 

Design goals: speed and low latency.


Design proposal for a new binary serialization format:

1. Use a parser design (similar to attoparsec) that supports partial parses.
   Use only a 

similar to attoparsec. Howev

1. Use chunked encoding for bounded types. Decoding is then factored into
   extracting a lazy bytestring from the chunk lengths and decoding this
   lazy bytestring efficiently.

2. Use a tagged encoding for recursive types.

-}
--
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
  [ bgroup "manyWord8s"
      [ bench "unpack" $ nf L.unpack word8Data 
      , benchParse     manyWord8sViaUnpack word8Data 
      , benchParse     parseManyWord8s word8Data
      , benchAtto      attoManyWord8s word8Data
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

  , bgroup "binaryWord8s"
      [ benchGet           getBinaryWord8s   binaryData
      , benchParse         parseBinaryWord8s binaryData
      , benchAtto          attoBinaryWord8s  binaryData
      , benchParseViaAtto  parseBinaryWord8s binaryData
      ] 

  , bgroup "binaryWord8s NOINLINE"
      [ benchGet           getBinaryWord8sNoInline   binaryData
      , benchParse         parseBinaryWord8sNoInline binaryData
      , benchAtto          attoBinaryWord8sNoInline  binaryData
      , benchParseViaAtto  parseBinaryWord8sNoInline binaryData
      ] 

  , bgroup "replicateNWord8s"
      [ benchGet     getNWord8s word8Data
      , benchParse   parseNWord8s word8Data
      , benchAtto    attoNWord8s word8Data
      ]
  ]
  where
    benchParse p inp = bench "attoget" $ nf (fst . runParser p) inp

    benchParseViaAtto p = benchAtto' "attoparsec-get" (toAttoparsec p)

    benchAtto = benchAtto' "attoparsec"
    benchAtto' name p inp = bench name $ nf (A.eitherResult . A.parse p) inp

    benchGet p inp = bench "binaryget" $ nf (B.runGet p) inp

