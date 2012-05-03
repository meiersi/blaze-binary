{-# LANGUAGE GADTs, PackageImports, ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark encoding and decoding speed.
module Main (main, testNewBinary) where

import           Prelude hiding (words)
import           Criterion.Main

import           Data.Blaze.Binary.Encoding (renderTextualUtf8, renderTagged)
import qualified Data.Blaze.Binary.Decoding  as Blaze (word8s, word8sSimple, runDecoder)
import qualified Data.ByteString             as S
import qualified Data.ByteString.Lazy        as L
import qualified Data.ByteString.Lazy.Char8  as LC8
import           Data.Serialize

import           Data.Binary (Binary)
import qualified Data.Binary as Binary

import qualified Data.Blaze.Binary as Blaze

import qualified Data.Sequence as Seq
import           Data.Tree
import           Data.Word


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

-- | The number of repetitions to consider.
nRepl :: Int
nRepl = 1000

-- We use NOINLINE to ensure that GHC has no chance of optimizing too much.

{-# NOINLINE intData #-}
intData :: Int -> [Int]
intData n = take n [0..]

{-# NOINLINE stringData #-}
stringData :: Int -> [String]
stringData n = take n $ cycle ["hello", "world"]

{-# NOINLINE seqIntData #-}
seqIntData :: Int -> Seq.Seq Int
seqIntData = Seq.fromList . intData

-- | Build a balanced binary tree.
{-# NOINLINE treeIntData #-}
treeIntData :: Int -> Tree Int
treeIntData n = 
   head $ go [0..n]  -- assuming n >= 0
  where
   go []  = []
   go [x] = [Node x []]
   go xs  =
       [Node r $ concatMap go [ls, rs]]
     where
       (ls, r:rs) = splitAt (length xs `div` 2) xs

testValue :: Int -> [Maybe (String, S.ByteString, [Int], Double)]
testValue n = replicate n $ Just 
    ("Haskell", S.pack [0xbe,0xef], [-2..1], 0.123 :: Double)

word8Data :: Int -> [Word8]
word8Data n = take n $ cycle [(0::Word8)..]

-- benchmarks
-------------

main :: IO ()
main = Criterion.Main.defaultMain $ 
    [ bgroup "decode"
      [ bench "blaze-binary: word8s" $ nf (Blaze.runDecoder Blaze.word8s) (Blaze.toByteString $ word8Data nRepl)
      , bench "blaze-binary: word8sSimple" $ nf (Blaze.runDecoder Blaze.word8sSimple) (Blaze.toByteString $ word8Data nRepl)
      , bench "cereal: word8s" $ nf (decodeLazy :: L.ByteString -> Either String [Word8]) (encodeLazy $ word8Data nRepl)
      , bench "binary: word8s" $ nf (Binary.decode :: L.ByteString -> [Word8]) (Binary.encode $ word8Data nRepl)
      ]
    , bgroup "encode"
      [ benchmarks "testValue "  id         (testValue nRepl)
      , benchmarks "Tree Int "  id         (treeIntData nRepl)
      , benchmarks "Seq Int "   id         (seqIntData nRepl)
      , benchmarks "[Int] "     id         (intData nRepl)
      , benchmarks "[String] "   id         (stringData nRepl)
      -- , benchmarks "[String] generated"  stringData nRepl
      ]
    ]
  where
    benchmarks :: (Binary a, Blaze.Binary a, Serialize a) 
               => String -> (b -> a) -> b -> Benchmark
    benchmarks name f x = bgroup (name ++ show nRepl)
      [ bench "blaze-binary" $ whnf (L.length . Blaze.toLazyByteString . f) x
      -- , bench "blaze-binary tagged" $ whnf (L.length . renderTagged . Blaze.encode . f) x andrea
      , bench "cereal" $ whnf (L.length . encodeLazy . f)  x
      , bench "binary" $ whnf (L.length . Binary.encode . f) x
      ]

-- | Testing the new binary encoding format.
testNewBinary :: Blaze.Binary a => a -> IO ()
testNewBinary x = 
  LC8.putStrLn $ renderTextualUtf8 $ Blaze.encode (x, Blaze.toLazyByteString x)


