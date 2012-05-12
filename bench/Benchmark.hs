{-# LANGUAGE CPP, GADTs, PackageImports, ScopedTypeVariables, BangPatterns #-}

#ifdef GENERICS
{-# LANGUAGE DeriveGeneric #-}
#endif

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
import           Data.Monoid ((<>))
import           Criterion.Main
import           Control.DeepSeq
import           Control.Applicative

import           Data.Blaze.Binary.Encoding (renderTextualUtf8, renderTagged)
import qualified Data.Blaze.Binary.Encoding as E (word8)
import qualified Data.Blaze.Binary.Decoding as D (word8)
import qualified Data.Blaze.Binary.Decoding       as Blaze (Decoder, runDecoder)
import qualified Data.Blaze.Binary.ParamDecoding  as ParamBlaze (Decoder, runDecoder, word8s, string)
import qualified Data.Blaze.Binary.IterDecoding   as IterBlaze (DStream, decodeWith, word8s, string, listOfWord8s )
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

#ifdef GENERICS
import GHC.Generics
#endif

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

word8sData :: Int -> [[Word8]]
word8sData n = take n $ cycle [[1..5], [101..105]]

charData :: Int -> String
charData n = take n ['\0'..]

-- benchmarks
-------------

main :: IO ()
main = Criterion.Main.defaultMain $
    [ bgroup ("decode (" ++ show nRepl ++ ")")
       -- [ bench "param-blaze-binary: word8s" $ nf 
       --     (benchParamDecoder ParamBlaze.word8s . S.copy) 
       --     (Blaze.toByteString $ word8Data nRepl)
       [ bench "iter-blaze-binary: word8s" $ nf 
           (benchIterDecoder IterBlaze.word8s) 
           (Blaze.toByteString $ word8Data nRepl)
       , bench "binary-cps: word8s" $ nf (Binary.decode :: L.ByteString -> [Word8]) (Binary.encode $ word8Data nRepl)
       , bench "iter-blaze-binary: [word8s]" $ nf 
           (benchIterDecoder IterBlaze.listOfWord8s) 
           (Blaze.toByteString $ word8sData nRepl)
       , bench "binary-cps: [word8s]" $ nf (Binary.decode :: L.ByteString -> [[Word8]]) (Binary.encode $ word8sData nRepl)
       -- , bench "attoparsec-noinline: word8s" $ nf 
       --     (benchAttoparsec attoBinaryWord8sNoInline)
       --     (Blaze.toByteString $ word8Data nRepl)
       -- , bench "param-blaze-binary: string" $ nf 
       --     (benchParamDecoder ParamBlaze.string) 
       --     (Blaze.toByteString $ charData nRepl)
       , bench "iter-blaze-binary: string" $ nf 
           (benchIterDecoder IterBlaze.string) 
           (Blaze.toByteString $ charData nRepl)
    --   , bench "blaze-binary: word8sSimple" $ nf (benchDecoder Blaze.word8sSimple) (Blaze.toByteString $ word8Data nRepl)
    --   , bench "cereal: word8s" $ nf (decodeLazy :: L.ByteString -> Either String [Word8]) (encodeLazy $ word8Data nRepl)
       , bench "binary-cps: string" $ nf (Binary.decode :: L.ByteString -> String) (Binary.encode $ charData nRepl)
       -- , bench "stream-blaze-binary: word8s" $ nf 
       --     (StreamBlaze.benchWord8s . S.copy)
       --     (Blaze.toByteString $ word8Data nRepl)
       -- , bench "blaze-binary: word8s" $ nf 
       --     (benchDecoder (Blaze.decode :: Blaze.Decoder [Word8]) . S.copy)
       --     (Blaze.toByteString $ word8Data nRepl)

       -- , bench "blaze-binary: string" $ nf 
       --     (benchDecoder (Blaze.decode :: Blaze.Decoder String))
       --     (Blaze.toByteString $ charData nRepl)
    --   , bench "blaze-binary: word8sSimple" $ nf (benchDecoder Blaze.word8sSimple) (Blaze.toByteString $ word8Data nRepl)
    --   , bench "cereal: word8s" $ nf (decodeLazy :: L.ByteString -> Either String [Word8]) (encodeLazy $ word8Data nRepl)
       -- , bench "attoparsec-inlined: word8s" $ nf 
       --     (benchAttoparsec attoBinaryWord8s)
       --     (Blaze.toByteString $ word8Data nRepl)
{- =======
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
>>>>>>> basvandijk/master -}
       ]

    , bgroup "encode"
      [ benchmarks "[Word8] "   id        (word8Data nRepl)
      , benchmarks "[[Word8]]"   id       (word8sData nRepl)
      , benchmarks "String "    id        (charData nRepl)
      , benchmarks "[String] "  id        (stringData nRepl)
      , benchmarks "testValue " id        (testValue nRepl)
      , benchmarks "Tree Int "  id        (treeIntData nRepl)
      , benchmarks "Seq Int "   id        (seqIntData nRepl)
      , benchmarks "[Int] "     id        (intData nRepl)
      ]

#ifdef GENERICS
    , bgroup "generic"
      [ bgroup "decode"
        [ bgroup "bigProduct"
          [ bench "generic" $ benchDecode bigProduct
          , bench "manual"  $ benchDecode bigProduct'
          ]
        , bgroup "bigSum"
          [ bgroup "0"
            [ bench "generic" $ benchDecode C00
            , bench "manual"  $ benchDecode C'00
            ]
          , bgroup "99"
            [ bench "generic" $ benchDecode C99
            , bench "manual"  $ benchDecode C'99
            ]
          ]
        ]
      , bgroup "encode"
        [ bgroup "bigProduct"
          [ bench "generic" $ nf (L.length . Blaze.toLazyByteString) bigProduct
          , bench "manual"  $ nf (L.length . Blaze.toLazyByteString) bigProduct'
          ]
        , bgroup "bigSum"
          [ bgroup "0"
            [ bench "generic" $ nf (L.length . Blaze.toLazyByteString) C00
            , bench "manual"  $ nf (L.length . Blaze.toLazyByteString) C'00
            ]
          , bgroup "99"
            [ bench "generic" $ nf (L.length . Blaze.toLazyByteString) C99
            , bench "manual"  $ nf (L.length . Blaze.toLazyByteString) C'99
            ]
          ]
        ]
      ]
#endif
    ]
  where
    benchDecode :: forall a. (NFData a, Blaze.Binary a) => a -> Pure
    benchDecode x = nf (benchDecoder (Blaze.decode :: Blaze.Decoder a)) (Blaze.toByteString x)

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
       -- , bench "cereal" $ nf (L.length . encodeLazy . f)  x
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

#ifdef GENERICS
------------------------------------------------------------------------------
-- Generics
------------------------------------------------------------------------------

data BigProduct = BigProduct   !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
  deriving Generic

instance NFData BigProduct

instance Blaze.Binary BigProduct

bigProduct :: BigProduct
bigProduct = BigProduct 00 01 02 03 04 05 06 07 08 09
                        10 11 12 13 14 15 16 17 18 19
                        20 21 22 23 24 25 26 27 28 29
                        30 31 32 33 34 35 36 37 38 39
                        40 41 42 43 44 45 46 47 48 49
                        50 51 52 53 54 55 56 57 58 59
                        60 61 62 63 64 65 66 67 68 69
                        70 71 72 73 74 75 76 77 78 79
                        80 81 82 83 84 85 86 87 88 89
                        90 91 92 93 94 95 96 97 98 99

data BigProduct' = BigProduct' !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int
                               !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int

instance NFData BigProduct'

instance Blaze.Binary BigProduct' where
    encode (BigProduct' i00 i01 i02 i03 i04 i05 i06 i07 i08 i09
                        i10 i11 i12 i13 i14 i15 i16 i17 i18 i19
                        i20 u21 i21 i22 i23 i24 i25 i26 i27 i28
                        i30 i31 i32 i33 i34 i35 i36 i37 i38 i39
                        i40 i41 i42 i43 i44 i45 i46 i47 i48 i49
                        i50 i51 i52 i53 i54 i55 i56 i57 i58 i59
                        i60 i61 i62 i63 i64 i65 i66 i67 i68 i69
                        i70 i71 i72 i73 i74 i75 i76 i77 i78 i79
                        i80 i81 i82 i83 i84 i85 i86 i87 i88 i89
                        i90 i91 i92 i93 i94 i95 i96 i97 i98 i99
           ) =

                Blaze.encode i00 <> Blaze.encode i01 <> Blaze.encode i02 <> Blaze.encode i03 <> Blaze.encode i04
             <> Blaze.encode i05 <> Blaze.encode i06 <> Blaze.encode i07 <> Blaze.encode i08 <> Blaze.encode i09
             <> Blaze.encode i10 <> Blaze.encode i11 <> Blaze.encode i12 <> Blaze.encode i13 <> Blaze.encode i14
             <> Blaze.encode i15 <> Blaze.encode i16 <> Blaze.encode i17 <> Blaze.encode i18 <> Blaze.encode i19
             <> Blaze.encode i20 <> Blaze.encode u21 <> Blaze.encode i21 <> Blaze.encode i22 <> Blaze.encode i23
             <> Blaze.encode i24 <> Blaze.encode i25 <> Blaze.encode i26 <> Blaze.encode i27 <> Blaze.encode i28
             <> Blaze.encode i30 <> Blaze.encode i31 <> Blaze.encode i32 <> Blaze.encode i33 <> Blaze.encode i34
             <> Blaze.encode i35 <> Blaze.encode i36 <> Blaze.encode i37 <> Blaze.encode i38 <> Blaze.encode i39
             <> Blaze.encode i40 <> Blaze.encode i41 <> Blaze.encode i42 <> Blaze.encode i43 <> Blaze.encode i44
             <> Blaze.encode i45 <> Blaze.encode i46 <> Blaze.encode i47 <> Blaze.encode i48 <> Blaze.encode i49
             <> Blaze.encode i50 <> Blaze.encode i51 <> Blaze.encode i52 <> Blaze.encode i53 <> Blaze.encode i54
             <> Blaze.encode i55 <> Blaze.encode i56 <> Blaze.encode i57 <> Blaze.encode i58 <> Blaze.encode i59
             <> Blaze.encode i60 <> Blaze.encode i61 <> Blaze.encode i62 <> Blaze.encode i63 <> Blaze.encode i64
             <> Blaze.encode i65 <> Blaze.encode i66 <> Blaze.encode i67 <> Blaze.encode i68 <> Blaze.encode i69
             <> Blaze.encode i70 <> Blaze.encode i71 <> Blaze.encode i72 <> Blaze.encode i73 <> Blaze.encode i74
             <> Blaze.encode i75 <> Blaze.encode i76 <> Blaze.encode i77 <> Blaze.encode i78 <> Blaze.encode i79
             <> Blaze.encode i80 <> Blaze.encode i81 <> Blaze.encode i82 <> Blaze.encode i83 <> Blaze.encode i84
             <> Blaze.encode i85 <> Blaze.encode i86 <> Blaze.encode i87 <> Blaze.encode i88 <> Blaze.encode i89
             <> Blaze.encode i90 <> Blaze.encode i91 <> Blaze.encode i92 <> Blaze.encode i93 <> Blaze.encode i94
             <> Blaze.encode i95 <> Blaze.encode i96 <> Blaze.encode i97 <> Blaze.encode i98 <> Blaze.encode i99

    decode = BigProduct' <$> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
                         <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode

bigProduct' :: BigProduct'
bigProduct' = BigProduct' 00 01 02 03 04 05 06 07 08 09
                          10 11 12 13 14 15 16 17 18 19
                          20 21 22 23 24 25 26 27 28 29
                          30 31 32 33 34 35 36 37 38 39
                          40 41 42 43 44 45 46 47 48 49
                          50 51 52 53 54 55 56 57 58 59
                          60 61 62 63 64 65 66 67 68 69
                          70 71 72 73 74 75 76 77 78 79
                          80 81 82 83 84 85 86 87 88 89
                          90 91 92 93 94 95 96 97 98 99

data BigSum = C00 | C01 | C02 | C03 | C04 | C05 | C06 | C07 | C08 | C09
            | C10 | C11 | C12 | C13 | C14 | C15 | C16 | C17 | C18 | C19
            | C20 | C21 | C22 | C23 | C24 | C25 | C26 | C27 | C28 | C29
            | C30 | C31 | C32 | C33 | C34 | C35 | C36 | C37 | C38 | C39
            | C40 | C41 | C42 | C43 | C44 | C45 | C46 | C47 | C48 | C49
            | C50 | C51 | C52 | C53 | C54 | C55 | C56 | C57 | C58 | C59
            | C60 | C61 | C62 | C63 | C64 | C65 | C66 | C67 | C68 | C69
            | C70 | C71 | C72 | C73 | C74 | C75 | C76 | C77 | C78 | C79
            | C80 | C81 | C82 | C83 | C84 | C85 | C86 | C87 | C88 | C89
            | C90 | C91 | C92 | C93 | C94 | C95 | C96 | C97 | C98 | C99
  deriving Generic

instance NFData BigSum

instance Blaze.Binary BigSum

data BigSum' = C'00 | C'01 | C'02 | C'03 | C'04 | C'05 | C'06 | C'07 | C'08 | C'09
             | C'10 | C'11 | C'12 | C'13 | C'14 | C'15 | C'16 | C'17 | C'18 | C'19
             | C'20 | C'21 | C'22 | C'23 | C'24 | C'25 | C'26 | C'27 | C'28 | C'29
             | C'30 | C'31 | C'32 | C'33 | C'34 | C'35 | C'36 | C'37 | C'38 | C'39
             | C'40 | C'41 | C'42 | C'43 | C'44 | C'45 | C'46 | C'47 | C'48 | C'49
             | C'50 | C'51 | C'52 | C'53 | C'54 | C'55 | C'56 | C'57 | C'58 | C'59
             | C'60 | C'61 | C'62 | C'63 | C'64 | C'65 | C'66 | C'67 | C'68 | C'69
             | C'70 | C'71 | C'72 | C'73 | C'74 | C'75 | C'76 | C'77 | C'78 | C'79
             | C'80 | C'81 | C'82 | C'83 | C'84 | C'85 | C'86 | C'87 | C'88 | C'89
             | C'90 | C'91 | C'92 | C'93 | C'94 | C'95 | C'96 | C'97 | C'98 | C'99

instance NFData BigSum'

instance Blaze.Binary BigSum' where
    encode C'00 = E.word8 00; encode C'01 = E.word8 01; encode C'02 = E.word8 02; encode C'03 = E.word8 03; encode C'04 = E.word8 04
    encode C'05 = E.word8 05; encode C'06 = E.word8 06; encode C'07 = E.word8 07; encode C'08 = E.word8 08; encode C'09 = E.word8 09
    encode C'10 = E.word8 10; encode C'11 = E.word8 11; encode C'12 = E.word8 12; encode C'13 = E.word8 13; encode C'14 = E.word8 14
    encode C'15 = E.word8 15; encode C'16 = E.word8 16; encode C'17 = E.word8 17; encode C'18 = E.word8 18; encode C'19 = E.word8 19
    encode C'20 = E.word8 20; encode C'21 = E.word8 21; encode C'22 = E.word8 22; encode C'23 = E.word8 23; encode C'24 = E.word8 24
    encode C'25 = E.word8 25; encode C'26 = E.word8 26; encode C'27 = E.word8 27; encode C'28 = E.word8 28; encode C'29 = E.word8 29
    encode C'30 = E.word8 30; encode C'31 = E.word8 31; encode C'32 = E.word8 32; encode C'33 = E.word8 33; encode C'34 = E.word8 34
    encode C'35 = E.word8 35; encode C'36 = E.word8 36; encode C'37 = E.word8 37; encode C'38 = E.word8 38; encode C'39 = E.word8 39
    encode C'40 = E.word8 40; encode C'41 = E.word8 41; encode C'42 = E.word8 42; encode C'43 = E.word8 43; encode C'44 = E.word8 44
    encode C'45 = E.word8 45; encode C'46 = E.word8 46; encode C'47 = E.word8 47; encode C'48 = E.word8 48; encode C'49 = E.word8 49
    encode C'50 = E.word8 50; encode C'51 = E.word8 51; encode C'52 = E.word8 52; encode C'53 = E.word8 53; encode C'54 = E.word8 54
    encode C'55 = E.word8 55; encode C'56 = E.word8 56; encode C'57 = E.word8 57; encode C'58 = E.word8 58; encode C'59 = E.word8 59
    encode C'60 = E.word8 60; encode C'61 = E.word8 61; encode C'62 = E.word8 62; encode C'63 = E.word8 63; encode C'64 = E.word8 64
    encode C'65 = E.word8 65; encode C'66 = E.word8 66; encode C'67 = E.word8 67; encode C'68 = E.word8 68; encode C'69 = E.word8 69
    encode C'70 = E.word8 70; encode C'71 = E.word8 71; encode C'72 = E.word8 72; encode C'73 = E.word8 73; encode C'74 = E.word8 74
    encode C'75 = E.word8 75; encode C'76 = E.word8 76; encode C'77 = E.word8 77; encode C'78 = E.word8 78; encode C'79 = E.word8 79
    encode C'80 = E.word8 80; encode C'81 = E.word8 81; encode C'82 = E.word8 82; encode C'83 = E.word8 83; encode C'84 = E.word8 84
    encode C'85 = E.word8 85; encode C'86 = E.word8 86; encode C'87 = E.word8 87; encode C'88 = E.word8 88; encode C'89 = E.word8 89
    encode C'90 = E.word8 90; encode C'91 = E.word8 91; encode C'92 = E.word8 92; encode C'93 = E.word8 93; encode C'94 = E.word8 94
    encode C'95 = E.word8 95; encode C'96 = E.word8 96; encode C'97 = E.word8 97; encode C'98 = E.word8 98; encode C'99 = E.word8 99

    decode = f <$> D.word8
        where
          f 00 = C'00; f 01= C'01; f 02= C'02; f 03= C'03; f 04= C'04; f 05= C'05; f 06= C'06; f 07= C'07; f 08= C'08; f 09= C'09
          f 10 = C'10; f 11= C'11; f 12= C'12; f 13= C'13; f 14= C'14; f 15= C'15; f 16= C'16; f 17= C'17; f 18= C'18; f 19= C'19
          f 20 = C'20; f 21= C'21; f 22= C'22; f 23= C'23; f 24= C'24; f 25= C'25; f 26= C'26; f 27= C'27; f 28= C'28; f 29= C'29
          f 30 = C'30; f 31= C'31; f 32= C'32; f 33= C'33; f 34= C'34; f 35= C'35; f 36= C'36; f 37= C'37; f 38= C'38; f 39= C'39
          f 40 = C'40; f 41= C'41; f 42= C'42; f 43= C'43; f 44= C'44; f 45= C'45; f 46= C'46; f 47= C'47; f 48= C'48; f 49= C'49
          f 50 = C'50; f 51= C'51; f 52= C'52; f 53= C'53; f 54= C'54; f 55= C'55; f 56= C'56; f 57= C'57; f 58= C'58; f 59= C'59
          f 60 = C'60; f 61= C'61; f 62= C'62; f 63= C'63; f 64= C'64; f 65= C'65; f 66= C'66; f 67= C'67; f 68= C'68; f 69= C'69
          f 70 = C'70; f 71= C'71; f 72= C'72; f 73= C'73; f 74= C'74; f 75= C'75; f 76= C'76; f 77= C'77; f 78= C'78; f 79= C'79
          f 80 = C'80; f 81= C'81; f 82= C'82; f 83= C'83; f 84= C'84; f 85= C'85; f 86= C'86; f 87= C'87; f 88= C'88; f 89= C'89
          f 90 = C'90; f 91= C'91; f 92= C'92; f 93= C'93; f 94= C'94; f 95= C'95; f 96= C'96; f 97= C'97; f 98= C'98; f 99= C'99
#endif
