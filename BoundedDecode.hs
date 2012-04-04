{-# LANGUAGE MagicHash, BangPatterns #-}
-- | Bounded decoding functions.
module Main where

import Data.Bits
import Data.Char
import Data.List (findIndex)
import Foreign
import GHC.Ptr
import Control.Monad

import qualified Data.ByteString.Internal as S

import qualified Data.ByteString.Char8 as S8

import Data.ByteString.Lex.Integral (readDecimal_, readHexadecimal)
import Criterion.Main

------------------------------------------------------------------------------
-- Primitives
------------------------------------------------------------------------------

data ResWord = ResWord {-# UNPACK #-} !(Ptr Word8)
                       {-# UNPACK #-} !Word

decTable, hexTable, octTable :: Ptr Word8
decTable = Ptr "\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128"#
hexTable = Ptr "\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\128\128\128\128\128\128\128\n\v\f\r\SO\SI\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\n\v\f\r\SO\SI\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128"#
octTable = Ptr "\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128"#

mkHexTable, mkOctTable, mkDecTable :: String
mkHexTable = mkTable ["0123456789abcdef","0123456789ABCDEF"]
mkOctTable = mkTable ["01234567"]
mkDecTable = mkTable ["0123456789"]

mkTable :: [String] -> String
mkTable digits = map (toDec . chr) [0..127]
  where
    toDec c = case msum $ map (findIndex (==c)) digits of
      Just i  -> chr i
      Nothing -> '\x80'

{-# INLINE primWordDec #-}
primWordDec :: Ptr Word8                -- ^ Digit translation table.
            -> (Word -> Word -> Word)   -- ^ Digit combination function.
            -> Ptr Word8                -- ^ First byte to read.
            -> Ptr Word8                -- ^ First byte after end.
            -> IO ResWord 
            -- -> IO Word
primWordDec digitTable combine !ip0 !ipe =
    go 0 ip0
  where
    go !w !ip
      | ip >= ipe = return $ ResWord ip w
      | otherwise = do
          asciiDigit <- peek ip
          rawDigit   <- peek (digitTable `plusPtr` fromIntegral asciiDigit)
          -- case fromIntegral asciiDigit - (48 :: Word) of
          --   digit | digit < 10 -> go (combine w digit) (ip `plusPtr` 1)
          --         | otherwise  -> return w -- ResWord ip w
          let digit = (fromIntegral (rawDigit :: Word8)) :: Word
          if testBit digit 7
            then return $ ResWord ip w
            else go (combine w digit) (ip `plusPtr` 1)

{-
wordDec :: S.ByteString -> Maybe (Word, S.ByteString)
wordDec (S.PS fpbuf off len) =
    S.inlinePerformIO $ withForeignPtr fpbuf $ \pbuf -> do
        let ip  = pbuf `plusPtr` off
            ipe = ip `plusPtr` len
        ResWord ip' w <- primWordDec decTable (\w d -> 10 * w + d) ip ipe
        if ip == ip'
          then return Nothing
          else do let !bs' = S.PS fpbuf (ip' `minusPtr` pbuf) 
                                        (ipe `minusPtr` ip')
                  return (Just (w, bs'))
-}

wordDec_ :: S.ByteString -> Word
wordDec_ (S.PS fpbuf off len) =
    S.inlinePerformIO $ withForeignPtr fpbuf $ \pbuf -> do
        let ip  = pbuf `plusPtr` off
            ipe = ip `plusPtr` len
        -- primWordDec decTable (\w d -> 10 * w + d) ip ipe
        ResWord ip' w <- primWordDec decTable (\w d -> 10 * w + d) ip ipe
        if ip == ip'
          then return 0
          else return w

{-# NOINLINE wordHex #-}
wordHex :: S.ByteString -> Maybe (Word, S.ByteString)
wordHex (S.PS fpbuf off len) =
    S.inlinePerformIO $ withForeignPtr fpbuf $ \pbuf -> do
        let ip  = pbuf `plusPtr` off
            ipe = ip `plusPtr` len
        -- primWordDec hexTable (\w d -> w `shiftL` 4 .|. d) ip ipe
        ResWord ip' w <- primWordDec hexTable (\w d -> w * 16 + d) ip ipe
        if ip == ip'
          then return Nothing
          else do let !bs' = S.PS fpbuf (ip' `minusPtr` pbuf) 
                                        (ipe `minusPtr` ip')
                  return (Just (w, bs'))

-- test :: String -> Maybe (Word, S8.ByteString)
-- test = wordDec . S8.pack


shortData :: S8.ByteString
shortData = S8.pack "12345abcdef 123123"

main :: IO ()
main = defaultMain
    [ bench "wordHex"     $ nf (maybe 0 fst . wordHex)     shortData
    , bench "readHexadecimal" $ nf ((maybe 0 fst . readHexadecimal) :: S8.ByteString -> Word64) shortData
    , bench "wordDec_"     $ nf wordDec_     shortData
    , bench "readDecimal_" $ nf (readDecimal_ :: S8.ByteString -> Word64) shortData
    ]
