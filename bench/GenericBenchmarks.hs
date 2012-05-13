{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}

-- |
-- Copyright   : (c) 2012 Simon Meier, Bas van Dijk
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark generic encoding and decoding speed.
module Main (main) where

import           Prelude hiding (words)
import           Data.Monoid ((<>))
import           Criterion.Main
import           Control.DeepSeq
import           Control.Applicative

import qualified Data.Blaze.Binary as Blaze
import qualified Data.Blaze.Binary.Encoding as E (word8)
import qualified Data.Blaze.Binary.Decoding as D (word8)
import qualified Data.Blaze.Binary.Decoding as Blaze (Decoder, runDecoder)
import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as L

import GHC.Generics

------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

main :: IO ()
main = Criterion.Main.defaultMain
       [ bgroup "decode"
         [ bgroup "bigProduct"
           [ bgroup "lazy"
             [ bench "generic" $ benchDecode bigProductL
             , bench "manual"  $ benchDecode bigProductL'
             ]
           , bgroup "strict"
             [ bench "generic" $ benchDecode bigProduct
             , bench "manual"  $ benchDecode bigProduct'
             ]
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
           [ bgroup "lazy"
             [ bench "generic" $ nf (L.length . Blaze.toLazyByteString) bigProductL
             , bench "manual"  $ nf (L.length . Blaze.toLazyByteString) bigProductL'
             ]
           , bgroup "strict"
             [ bench "generic" $ nf (L.length . Blaze.toLazyByteString) bigProduct
             , bench "manual"  $ nf (L.length . Blaze.toLazyByteString) bigProduct'
             ]
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
  where
    benchDecode :: forall a. (NFData a, Blaze.Binary a) => a -> Pure
    benchDecode x = nf (benchDecoder (Blaze.decode :: Blaze.Decoder a)) (Blaze.toByteString x)

    benchDecoder :: Blaze.Decoder a -> S.ByteString -> a
    benchDecoder d bs = case Blaze.runDecoder d bs of
      Left msg -> error msg
      Right x  -> x

------------------------------------------------------------------------------
-- Big strict products
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
                        i20 i21 i22 i23 i24 i25 i26 i27 i28 i29
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
             <> Blaze.encode i20 <> Blaze.encode i21 <> Blaze.encode i22 <> Blaze.encode i23 <> Blaze.encode i24
             <> Blaze.encode i25 <> Blaze.encode i26 <> Blaze.encode i27 <> Blaze.encode i28 <> Blaze.encode i29
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

--------------------------------------------------------------------------------
-- Big lazy products
--------------------------------------------------------------------------------

data BigProductL = BigProductL   Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
  deriving Generic

instance NFData BigProductL where
    rnf (BigProductL i00 i01 i02 i03 i04 i05 i06 i07 i08 i09
                     i10 i11 i12 i13 i14 i15 i16 i17 i18 i19
                     i20 i21 i22 i23 i24 i25 i26 i27 i28 i29
                     i30 i31 i32 i33 i34 i35 i36 i37 i38 i39
                     i40 i41 i42 i43 i44 i45 i46 i47 i48 i49
                     i50 i51 i52 i53 i54 i55 i56 i57 i58 i59
                     i60 i61 i62 i63 i64 i65 i66 i67 i68 i69
                     i70 i71 i72 i73 i74 i75 i76 i77 i78 i79
                     i80 i81 i82 i83 i84 i85 i86 i87 i88 i89
                     i90 i91 i92 i93 i94 i95 i96 i97 i98 i99
        ) = rnf i00 `seq` rnf i01 `seq` rnf i02 `seq` rnf i03 `seq` rnf i04 `seq` rnf i05 `seq` rnf i06 `seq` rnf i07 `seq` rnf i08 `seq` rnf i09 `seq`
            rnf i10 `seq` rnf i11 `seq` rnf i12 `seq` rnf i13 `seq` rnf i14 `seq` rnf i15 `seq` rnf i16 `seq` rnf i17 `seq` rnf i18 `seq` rnf i19 `seq`
            rnf i20 `seq` rnf i21 `seq` rnf i22 `seq` rnf i23 `seq` rnf i24 `seq` rnf i25 `seq` rnf i26 `seq` rnf i27 `seq` rnf i28 `seq` rnf i29 `seq`
            rnf i30 `seq` rnf i31 `seq` rnf i32 `seq` rnf i33 `seq` rnf i34 `seq` rnf i35 `seq` rnf i36 `seq` rnf i37 `seq` rnf i38 `seq` rnf i39 `seq`
            rnf i40 `seq` rnf i41 `seq` rnf i42 `seq` rnf i43 `seq` rnf i44 `seq` rnf i45 `seq` rnf i46 `seq` rnf i47 `seq` rnf i48 `seq` rnf i49 `seq`
            rnf i50 `seq` rnf i51 `seq` rnf i52 `seq` rnf i53 `seq` rnf i54 `seq` rnf i55 `seq` rnf i56 `seq` rnf i57 `seq` rnf i58 `seq` rnf i59 `seq`
            rnf i60 `seq` rnf i61 `seq` rnf i62 `seq` rnf i63 `seq` rnf i64 `seq` rnf i65 `seq` rnf i66 `seq` rnf i67 `seq` rnf i68 `seq` rnf i69 `seq`
            rnf i70 `seq` rnf i71 `seq` rnf i72 `seq` rnf i73 `seq` rnf i74 `seq` rnf i75 `seq` rnf i76 `seq` rnf i77 `seq` rnf i78 `seq` rnf i79 `seq`
            rnf i80 `seq` rnf i81 `seq` rnf i82 `seq` rnf i83 `seq` rnf i84 `seq` rnf i85 `seq` rnf i86 `seq` rnf i87 `seq` rnf i88 `seq` rnf i89 `seq`
            rnf i90 `seq` rnf i91 `seq` rnf i92 `seq` rnf i93 `seq` rnf i94 `seq` rnf i95 `seq` rnf i96 `seq` rnf i97 `seq` rnf i98 `seq` rnf i99

instance Blaze.Binary BigProductL

bigProductL :: BigProductL
bigProductL = BigProductL 00 01 02 03 04 05 06 07 08 09
                          10 11 12 13 14 15 16 17 18 19
                          20 21 22 23 24 25 26 27 28 29
                          30 31 32 33 34 35 36 37 38 39
                          40 41 42 43 44 45 46 47 48 49
                          50 51 52 53 54 55 56 57 58 59
                          60 61 62 63 64 65 66 67 68 69
                          70 71 72 73 74 75 76 77 78 79
                          80 81 82 83 84 85 86 87 88 89
                          90 91 92 93 94 95 96 97 98 99

data BigProductL' = BigProductL' Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int
                                 Int Int Int Int Int Int Int Int Int Int

instance NFData BigProductL' where
    rnf (BigProductL' i00 i01 i02 i03 i04 i05 i06 i07 i08 i09
                      i10 i11 i12 i13 i14 i15 i16 i17 i18 i19
                      i20 i21 i22 i23 i24 i25 i26 i27 i28 i29
                      i30 i31 i32 i33 i34 i35 i36 i37 i38 i39
                      i40 i41 i42 i43 i44 i45 i46 i47 i48 i49
                      i50 i51 i52 i53 i54 i55 i56 i57 i58 i59
                      i60 i61 i62 i63 i64 i65 i66 i67 i68 i69
                      i70 i71 i72 i73 i74 i75 i76 i77 i78 i79
                      i80 i81 i82 i83 i84 i85 i86 i87 i88 i89
                      i90 i91 i92 i93 i94 i95 i96 i97 i98 i99
        ) = rnf i00 `seq` rnf i01 `seq` rnf i02 `seq` rnf i03 `seq` rnf i04 `seq` rnf i05 `seq` rnf i06 `seq` rnf i07 `seq` rnf i08 `seq` rnf i09 `seq`
            rnf i10 `seq` rnf i11 `seq` rnf i12 `seq` rnf i13 `seq` rnf i14 `seq` rnf i15 `seq` rnf i16 `seq` rnf i17 `seq` rnf i18 `seq` rnf i19 `seq`
            rnf i20 `seq` rnf i21 `seq` rnf i22 `seq` rnf i23 `seq` rnf i24 `seq` rnf i25 `seq` rnf i26 `seq` rnf i27 `seq` rnf i28 `seq` rnf i29 `seq`
            rnf i30 `seq` rnf i31 `seq` rnf i32 `seq` rnf i33 `seq` rnf i34 `seq` rnf i35 `seq` rnf i36 `seq` rnf i37 `seq` rnf i38 `seq` rnf i39 `seq`
            rnf i40 `seq` rnf i41 `seq` rnf i42 `seq` rnf i43 `seq` rnf i44 `seq` rnf i45 `seq` rnf i46 `seq` rnf i47 `seq` rnf i48 `seq` rnf i49 `seq`
            rnf i50 `seq` rnf i51 `seq` rnf i52 `seq` rnf i53 `seq` rnf i54 `seq` rnf i55 `seq` rnf i56 `seq` rnf i57 `seq` rnf i58 `seq` rnf i59 `seq`
            rnf i60 `seq` rnf i61 `seq` rnf i62 `seq` rnf i63 `seq` rnf i64 `seq` rnf i65 `seq` rnf i66 `seq` rnf i67 `seq` rnf i68 `seq` rnf i69 `seq`
            rnf i70 `seq` rnf i71 `seq` rnf i72 `seq` rnf i73 `seq` rnf i74 `seq` rnf i75 `seq` rnf i76 `seq` rnf i77 `seq` rnf i78 `seq` rnf i79 `seq`
            rnf i80 `seq` rnf i81 `seq` rnf i82 `seq` rnf i83 `seq` rnf i84 `seq` rnf i85 `seq` rnf i86 `seq` rnf i87 `seq` rnf i88 `seq` rnf i89 `seq`
            rnf i90 `seq` rnf i91 `seq` rnf i92 `seq` rnf i93 `seq` rnf i94 `seq` rnf i95 `seq` rnf i96 `seq` rnf i97 `seq` rnf i98 `seq` rnf i99

instance Blaze.Binary BigProductL' where
    encode (BigProductL' i00 i01 i02 i03 i04 i05 i06 i07 i08 i09
                         i10 i11 i12 i13 i14 i15 i16 i17 i18 i19
                         i20 i21 i22 i23 i24 i25 i26 i27 i28 i29
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
             <> Blaze.encode i20 <> Blaze.encode i21 <> Blaze.encode i22 <> Blaze.encode i23 <> Blaze.encode i24
             <> Blaze.encode i25 <> Blaze.encode i26 <> Blaze.encode i27 <> Blaze.encode i28 <> Blaze.encode i29
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

    decode = BigProductL' <$> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode <*> Blaze.decode
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

bigProductL' :: BigProductL'
bigProductL' = BigProductL' 00 01 02 03 04 05 06 07 08 09
                            10 11 12 13 14 15 16 17 18 19
                            20 21 22 23 24 25 26 27 28 29
                            30 31 32 33 34 35 36 37 38 39
                            40 41 42 43 44 45 46 47 48 49
                            50 51 52 53 54 55 56 57 58 59
                            60 61 62 63 64 65 66 67 68 69
                            70 71 72 73 74 75 76 77 78 79
                            80 81 82 83 84 85 86 87 88 89
                            90 91 92 93 94 95 96 97 98 99

--------------------------------------------------------------------------------
-- Big sums
--------------------------------------------------------------------------------

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

    decode = D.word8 >>= \tag ->
               case tag of
                 00 -> return C'00; 01 -> return C'01; 02 -> return C'02; 03 -> return C'03; 04 -> return C'04
                 05 -> return C'05; 06 -> return C'06; 07 -> return C'07; 08 -> return C'08; 09 -> return C'09
                 10 -> return C'10; 11 -> return C'11; 12 -> return C'12; 13 -> return C'13; 14 -> return C'14
                 15 -> return C'15; 16 -> return C'16; 17 -> return C'17; 18 -> return C'18; 19 -> return C'19
                 20 -> return C'20; 21 -> return C'21; 22 -> return C'22; 23 -> return C'23; 24 -> return C'24
                 25 -> return C'25; 26 -> return C'26; 27 -> return C'27; 28 -> return C'28; 29 -> return C'29
                 30 -> return C'30; 31 -> return C'31; 32 -> return C'32; 33 -> return C'33; 34 -> return C'34
                 35 -> return C'35; 36 -> return C'36; 37 -> return C'37; 38 -> return C'38; 39 -> return C'39
                 40 -> return C'40; 41 -> return C'41; 42 -> return C'42; 43 -> return C'43; 44 -> return C'44
                 45 -> return C'45; 46 -> return C'46; 47 -> return C'47; 48 -> return C'48; 49 -> return C'49
                 50 -> return C'50; 51 -> return C'51; 52 -> return C'52; 53 -> return C'53; 54 -> return C'54
                 55 -> return C'55; 56 -> return C'56; 57 -> return C'57; 58 -> return C'58; 59 -> return C'59
                 60 -> return C'60; 61 -> return C'61; 62 -> return C'62; 63 -> return C'63; 64 -> return C'64
                 65 -> return C'65; 66 -> return C'66; 67 -> return C'67; 68 -> return C'68; 69 -> return C'69
                 70 -> return C'70; 71 -> return C'71; 72 -> return C'72; 73 -> return C'73; 74 -> return C'74
                 75 -> return C'75; 76 -> return C'76; 77 -> return C'77; 78 -> return C'78; 79 -> return C'79
                 80 -> return C'80; 81 -> return C'81; 82 -> return C'82; 83 -> return C'83; 84 -> return C'84
                 85 -> return C'85; 86 -> return C'86; 87 -> return C'87; 88 -> return C'88; 89 -> return C'89
                 90 -> return C'90; 91 -> return C'91; 92 -> return C'92; 93 -> return C'93; 94 -> return C'94
                 95 -> return C'95; 96 -> return C'96; 97 -> return C'97; 98 -> return C'98; 99 -> return C'99
                 _  -> fail "Unknown tag"
