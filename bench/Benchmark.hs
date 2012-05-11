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
import           Control.DeepSeq
import           Control.Applicative

import           Data.Blaze.Binary.Encoding (renderTextualUtf8, renderTagged)
import qualified Data.Blaze.Binary.Decoding       as Blaze (Decoder, runDecoder)
import qualified Data.Blaze.Binary.ParamDecoding  as ParamBlaze (Decoder, runDecoder, word8s, string)
import qualified Data.Blaze.Binary.IterDecoding   as IterBlaze (DStream, decodeWith, word8s, string )
import qualified Data.Blaze.Binary.StreamDecoding as StreamBlaze (benchWord8s)
import qualified Data.ByteString             as S
import qualified Data.ByteString.Internal    as S
import qualified Data.ByteString.Lazy        as L
import qualified Data.ByteString.Lazy.Char8  as LC8
import           Data.Serialize

import           Data.Binary (Binary)
import qualified Data.Binary as Binary

import qualified Data.Blaze.Binary as Blaze

import qualified Data.Sequence as Seq
import           Data.Tree
import           Data.Word
import qualified Data.Foldable as F (toList)

import qualified Data.Attoparsec as A


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

charData :: Int -> String
charData n = take n ['\0'..]

-- benchmarks
-------------

main :: IO ()
main = Criterion.Main.defaultMain $ 
    [ bgroup ("decode (" ++ show nRepl ++ ")")
       [ bench "param-blaze-binary: word8s" $ nf 
           (benchParamDecoder ParamBlaze.word8s . S.copy) 
           (Blaze.toByteString $ word8Data nRepl)
       , bench "iter-blaze-binary: word8s" $ nf 
           (benchIterDecoder IterBlaze.word8s) 
           (Blaze.toByteString $ word8Data nRepl)
       , bench "binary: word8s" $ nf (Binary.decode :: L.ByteString -> [Word8]) (Binary.encode $ word8Data nRepl)
       , bench "attoparsec-noinline: word8s" $ nf 
           (benchAttoparsec attoBinaryWord8sNoInline)
           (Blaze.toByteString $ word8Data nRepl)
       , bench "param-blaze-binary: string" $ nf 
           (benchParamDecoder ParamBlaze.string) 
           (Blaze.toByteString $ charData nRepl)
       , bench "iter-blaze-binary: string" $ nf 
           (benchIterDecoder IterBlaze.string) 
           (Blaze.toByteString $ charData nRepl)
    --   , bench "blaze-binary: word8sSimple" $ nf (benchDecoder Blaze.word8sSimple) (Blaze.toByteString $ word8Data nRepl)
    --   , bench "cereal: word8s" $ nf (decodeLazy :: L.ByteString -> Either String [Word8]) (encodeLazy $ word8Data nRepl)
       , bench "binary: string" $ nf (Binary.decode :: L.ByteString -> String) (Binary.encode $ charData nRepl)
       , bench "stream-blaze-binary: word8s" $ nf 
           (StreamBlaze.benchWord8s . S.copy)
           (Blaze.toByteString $ word8Data nRepl)
       , bench "blaze-binary: word8s" $ nf 
           (benchDecoder (Blaze.decode :: Blaze.Decoder [Word8]) . S.copy)
           (Blaze.toByteString $ word8Data nRepl)

       , bench "blaze-binary: string" $ nf 
           (benchDecoder (Blaze.decode :: Blaze.Decoder String))
           (Blaze.toByteString $ charData nRepl)
    --   , bench "blaze-binary: word8sSimple" $ nf (benchDecoder Blaze.word8sSimple) (Blaze.toByteString $ word8Data nRepl)
    --   , bench "cereal: word8s" $ nf (decodeLazy :: L.ByteString -> Either String [Word8]) (encodeLazy $ word8Data nRepl)
       , bench "attoparsec-inlined: word8s" $ nf 
           (benchAttoparsec attoBinaryWord8s)
           (Blaze.toByteString $ word8Data nRepl)
       ]
        
    , bgroup "encode"
      [ benchmarks "String "   id          (charData nRepl)
      , benchmarks "[String] "   id        (stringData nRepl)
      , benchmarks "testValue "  id        (testValue nRepl)
      , benchmarks "Tree Int "  id         (treeIntData nRepl)
      , benchmarks "Seq Int "   id         (seqIntData nRepl)
      , benchmarks "[Int] "     id         (intData nRepl)
      ]
    ]
  where
    benchAttoparsec :: A.Parser a -> S.ByteString -> a
    benchAttoparsec p bs = case A.eitherResult $ A.parse p bs of
      Left msg -> error msg
      Right x  -> x

    benchDecoder :: Blaze.Decoder a -> S.ByteString -> a
    benchDecoder d bs = case Blaze.runDecoder d bs of
      Left msg -> error msg
      Right x  -> x

    benchParamDecoder :: ParamBlaze.Decoder a -> S.ByteString -> a
    benchParamDecoder d bs = case ParamBlaze.runDecoder d bs of
      Left msg -> error msg
      Right x  -> x

    benchIterDecoder :: IterBlaze.DStream a -> S.ByteString -> a
    benchIterDecoder d bs = case IterBlaze.decodeWith d bs of
      Left msg -> error msg
      Right x  -> x

    benchmarks :: forall a b. (Binary a, Blaze.Binary a, Serialize a, NFData a) 
               => String -> (b -> a) -> b -> Benchmark
    benchmarks name f x = bgroup (name ++ show nRepl)
     -- [ bgroup "decode"
     --   [ bench "blaze-binary" $ nf (benchDecoder Blaze.decode :: S.ByteString -> a) (Blaze.toByteString $ f x)
     --   -- , bench "blaze-binary tagged" $ whnf (L.length . renderTagged . Blaze.encode . f) x andrea
     --   , bench "cereal" $ nf (decodeLazy :: L.ByteString -> Either String a) (encodeLazy $ f x)
     --   , bench "binary" $ nf (Binary.decode :: L.ByteString -> a) (Binary.encode $ f  x)
     --   ]
     --, bgroup "encode"
       [ bench "blaze-binary" $ nf (L.length . Blaze.toLazyByteString . f) x
       -- , bench "blaze-binary tagged" $ whnf (L.length . renderTagged . Blaze.encode . f) x andrea
       , bench "cereal" $ nf (L.length . encodeLazy . f)  x
       , bench "binary" $ nf (L.length . Binary.encode . f) x
       ]
      --]

-- | Testing the new binary encoding format.
testNewBinary :: Blaze.Binary a => a -> IO ()
testNewBinary x = 
  LC8.putStrLn $ renderTextualUtf8 $ Blaze.encode (x, Blaze.toLazyByteString x)


instance NFData S.ByteString where
    rnf (S.PS _ _ _) = ()

instance NFData a => NFData (Seq.Seq a) where
    rnf = rnf . F.toList

------------------------------------------------------------------------------
-- Attoparsec
------------------------------------------------------------------------------

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

