{-# LANGUAGE CPP, BangPatterns, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Blaze.Binary.Encoder
-- Copyright   : 2012, Simon Meier <iridcode@gmail.com>
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   :
-- Portability : portable
--
-- Binary encoding of Haskell values.
-- Encoding Haskell values to streams of primtive values that can be
-- efficiently converted to sequenes of bytes.
--
-----------------------------------------------------------------------------
module Data.Blaze.Binary.Encoder (

    -- * 'Stream's of primitive values
      Stream
    , render

    -- * 'Encoder's
    , Encoder
    , runEncoder

    -- ** Primitive values
    , word
    , word8
    , word16
    , word32
    , word64

    , int
    , int8
    , int16
    , int32
    , int64

    , integer
    , float
    , double

    , char

    , byteString

    -- ** Combinators
    , encodeList

    -- * Debugging Support
    -- , renderTextualUtf8

    -- * Convenience export
    , (<>)

  ) where

import Prelude hiding (putChar)

import qualified Data.ByteString                                     as S
-- import qualified Data.ByteString.Char8                               as SC8
-- import qualified Data.ByteString.Lazy                                as L
import qualified Data.ByteString.Lazy.Builder                        as B
-- import qualified Data.ByteString.Lazy.Builder.ASCII                  as B
-- import qualified Data.ByteString.Lazy.Builder.Extras                 as B
import qualified Data.ByteString.Lazy.Builder.Internal               as B
import qualified Data.ByteString.Lazy.Builder.BasicEncoding          as E
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Internal as E
import           Data.Monoid
import           Data.Foldable (foldMap)
import           Data.Word
import           Data.Int
import           Foreign.Ptr


------------------------------------------------------------------------------
-- 'Stream's of primitive values
------------------------------------------------------------------------------

-- | A stream of primitive values. Use its 'Monoid' instance for
-- concatenation.
newtype Stream = Stream { toStreamRep :: StreamRep -> StreamRep }

-- | The representation for a stream of primitive values.
data StreamRep =
       EChar           {-# UNPACK #-} !Char         StreamRep
     | EWord           {-# UNPACK #-} !Word         StreamRep
     | EWord8          {-# UNPACK #-} !Word8        StreamRep
     | EWord16         {-# UNPACK #-} !Word16       StreamRep
     | EWord32         {-# UNPACK #-} !Word32       StreamRep
     | EWord64         {-# UNPACK #-} !Word64       StreamRep
     | EInt            {-# UNPACK #-} !Int          StreamRep
     | EInt8           {-# UNPACK #-} !Int8         StreamRep
     | EInt16          {-# UNPACK #-} !Int16        StreamRep
     | EInt32          {-# UNPACK #-} !Int32        StreamRep
     | EInt64          {-# UNPACK #-} !Int64        StreamRep
     | EFloat          {-# UNPACK #-} !Float        StreamRep
     | EDouble         {-# UNPACK #-} !Double       StreamRep
     | EInteger                       !Integer      StreamRep
     | EByteString                    !S.ByteString StreamRep
     -- Easily supportable, but not used currently. This provides a neat
     -- escape hatch for implementing your own encoder.
     -- | EBuilder                       !B.Builder    StreamRep
     | EEmpty


-- Instances
------------

instance Monoid Stream where
  {-# INLINE mempty #-}
  mempty                 = Stream id
  {-# INLINE mappend #-}
  b1 `mappend` b2        = Stream (toStreamRep b1 . toStreamRep b2)
  {-# INLINE mconcat #-}
  mconcat                = foldr mappend mempty

-- Rendering 'Stream's
----------------------

-- | Binary encode a 'Stream' to a lazy bytestring 'B.Builder'.
{-# INLINE renderWith #-}
renderWith :: Word8 -- ^ Format identifier.
           -> E.BoundedEncoding Word8 -> E.BoundedEncoding Word16 -> E.BoundedEncoding Word32 -> E.BoundedEncoding Word64 -> E.BoundedEncoding Word
           -> E.BoundedEncoding Int8  -> E.BoundedEncoding Int16  -> E.BoundedEncoding Int32  -> E.BoundedEncoding Int64  -> E.BoundedEncoding Int
           -> E.BoundedEncoding Char
           -> E.BoundedEncoding Float -> E.BoundedEncoding Double
           -> (Integer -> B.Builder)
           -> (S.ByteString -> B.Builder)
           -> (B.Builder -> B.Builder)
           -> Stream
           -> B.Builder
renderWith formatId w8 w16 w32 w64 w i8 i16 i32 i64 i c f d ibig bs _b =
    -- take care that inlining is possible once all encodings are fixed
    \vs0 -> prefix <> B.builder (step (toStreamRep vs0 EEmpty))
  where
    prefix = B.word8 0xce <> B.word8 0xbb <> B.word8 0x2e <> B.word8 formatId

    step vs1 k (B.BufferRange op0 ope0) =
        go vs1 op0
      where
        go vs !op
          | op `plusPtr` bound <= ope0 = case vs of
              EEmpty            -> k (B.BufferRange op ope0)
              EWord8  x vs'     -> E.runB w8  x op >>= go vs'
              EWord16 x vs'     -> E.runB w16 x op >>= go vs'
              EWord32 x vs'     -> E.runB w32 x op >>= go vs'
              EWord64 x vs'     -> E.runB w64 x op >>= go vs'
              EWord   x vs'     -> E.runB w   x op >>= go vs'
              EInt8   x vs'     -> E.runB i8  x op >>= go vs'
              EInt16  x vs'     -> E.runB i16 x op >>= go vs'
              EInt32  x vs'     -> E.runB i32 x op >>= go vs'
              EInt64  x vs'     -> E.runB i64 x op >>= go vs'
              EInt    x vs'     -> E.runB i   x op >>= go vs'
              EChar   x vs'     -> E.runB c   x op >>= go vs'
              EFloat  x vs'     -> E.runB f   x op >>= go vs'
              EDouble x vs'     -> E.runB d   x op >>= go vs'
              EInteger x vs'    -> B.runBuilderWith (ibig x) (step vs' k) (B.BufferRange op ope0)
              EByteString x vs' -> B.runBuilderWith (bs x)   (step vs' k) (B.BufferRange op ope0)
              -- EBuilder x vs'    -> B.runBuilderWith (b x)    (step vs' k) (B.BufferRange op ope0)
          | otherwise = return $ B.bufferFull bound op (step vs k)

    bound = max' w8 $ max' w16 $ max' w32 $ max' w64 $ max' w $
            max' i8 $ max' i16 $ max' i32 $ max' i64 $ max' i $
            max' c  $ max' f $ E.sizeBound d

    {-# INLINE max' #-}
    max' e = max (E.sizeBound e)

-- | Encode a 'Stream' to a lazy bytestring 'B.Builder' using a format
-- optimized for maximal throughput on 64-bit, x86 machines.
render :: Stream -> B.Builder
render = renderWith
    0x00 -- throughput, untagged
    (fe E.word8) (fe E.word16LE) (fe E.word32LE) (fe E.word64LE) (fe (fromIntegral E.>$< E.word64LE))
    (fe E.int8)  (fe E.int16LE)  (fe E.int32LE)  (fe E.int64LE)  (fe (fromIntegral E.>$< E.int64LE))
    E.charUtf8 (fe E.floatLE) (fe E.doubleLE)
    (error "render: integer: implement")
    (\x -> B.int64LE (fromIntegral $ S.length x) <> B.byteString x)
    id
  where
    {-# INLINE fe #-}
    fe = E.fromF

{- To be reactivated

-- | Binary encode a 'Stream' to a lazy bytestring 'B.Builder' using a
-- compact Base-128 encoding for integers and words.
renderCompact :: Stream -> B.Builder
renderCompact = renderWith
    (E.fromF E.word8) E.word16Base128LE      E.word32Base128LE      E.word64Base128LE
    E.wordBase128LE
    (E.fromF E.int8)  E.int16ZigZagBase128LE E.int32ZigZagBase128LE E.int64ZigZagBase128LE
    E.intZigZagBase128LE
    E.charUtf8
    (E.fromF E.floatLE) (E.fromF E.doubleLE)
    (error "render: integer: implement")
    (\x -> E.encodeWithB E.intZigZagBase128LE (S.length x) <> B.byteString x)
    id


-- | Binary encode a 'Stream' to a lazy bytestring 'B.Builder' using a tagged
-- format that allows to reconstruct the value stream.
renderTagged :: Stream -> L.ByteString
renderTagged =
    B.toLazyByteString
  . renderWith
      (tf 0 E.word8) (tf 1 E.word16LE) (tf 2 E.word32LE) (tf 3 E.word64LE) (tf 4 (fromIntegral E.>$< E.word64LE))
      (tf 5 E.int8)  (tf 6 E.int16LE)  (tf 7 E.int32LE)  (tf 8 E.int64LE)  (tf 9 (fromIntegral E.>$< E.int64LE))
      (tb 10 E.charUtf8) (tf 11 E.floatLE) (tf 12 E.doubleLE)
      (error "render: integer: implement")
      ((B.word8 14 <>) . B.byteString)
      (B.word8 15 <>)
  where
    {-# INLINE tf #-}
    tf t fe = tb t (E.fromF fe)

    {-# INLINE tb #-}
    tb t fb = (,) t E.>$< E.fromF E.word8 `E.pairB` fb


renderTextualUtf8 :: Stream -> L.ByteString
renderTextualUtf8 vs0 =
    B.toLazyByteString $ go (toStreamRep vs0 EEmpty)
  where
    go EEmpty                  = mempty
    go (EWord8 1 (EChar x vs)) = line "w8,c  1," (B.charUtf8 x) vs
    go (EWord8 1 (EWord x vs)) = line "w8,w  1," (B.wordDec x)  vs
    go (EWord8 1 (EInt  x vs)) = line "w8,i  1," (B.intDec x)   vs
    go (EInt l (EByteString x vs))
      | l > 0                  = line "i,bs  " (B.intDec l <> B.char8 ',' <> B.byteStringHexFixed x) vs
    go (EWord8  x vs)          = line "w8    " (B.word8Dec x)  vs
    go (EWord16 x vs)          = line "w16   " (B.word16Dec x) vs
    go (EWord32 x vs)          = line "w32   " (B.word32Dec x) vs
    go (EWord64 x vs)          = line "w64   " (B.word64Dec x) vs
    go (EWord   x vs)          = line "w     " (B.wordDec x)   vs
    go (EInt8  x vs)           = line "i8    " (B.int8Dec x)  vs
    go (EInt16 x vs)           = line "i16   " (B.int16Dec x) vs
    go (EInt32 x vs)           = line "i32   " (B.int32Dec x) vs
    go (EInt64 x vs)           = line "i64   " (B.int64Dec x) vs
    go (EInt   x vs)           = line "i     " (B.intDec x)   vs
    go (EChar   x vs)          = line "c     " (B.charUtf8 x)  vs
    go (EFloat  x vs)          = line "f     " (B.floatDec x)  vs
    go (EDouble x vs)          = line "d     " (B.doubleDec x) vs
    go (EInteger x vs)         = line "I     " (B.integerDec x) vs
    go (EByteString x vs)      = line "bs    " (B.byteStringHexFixed x) vs
    go (EBuilder x vs)         = line "B     " (B.lazyByteStringHexFixed $ B.toLazyByteString x) vs

    line :: SC8.ByteString -> B.Builder -> StreamRep -> B.Builder
    line pre b vs = B.byteStringCopy pre <> b <> B.char8 '\n' <> go vs
-}

------------------------------------------------------------------------------
-- Encoders
------------------------------------------------------------------------------

-- | An 'Encoder' encodes a Haskell value as 'Stream' of primitive values,
-- which can then be rendered as a sequence of bytes using different formats
-- for encoding the individual primitive values.
type Encoder t = t -> Stream


-- Execution
------------

-- | Run an encoder, i.e., use it to encode a value to a sequence of bytes.
runEncoder :: Encoder t -> t -> B.Builder
runEncoder = \e x -> render (e x)


-- Encoding primitive values
----------------------------

-- | Encode a 'Float' value.
{-# INLINE float #-}
float :: Encoder Float
float = Stream . EFloat

-- | Encode a 'Double' value.
{-# INLINE double #-}
double :: Encoder Double
double = Stream . EDouble

-- | Encode a 'Integer' value.
{-# INLINE integer #-}
integer :: Encoder Integer
integer = Stream . EInteger

-- | Encode a 'Word' value.
{-# INLINE word #-}
word :: Encoder Word
word = Stream . EWord

-- | Encode a 'Word8' value.
{-# INLINE word8 #-}
word8 :: Encoder Word8
word8 = Stream . EWord8

-- | Encode a 'Word16' value.
{-# INLINE word16 #-}
word16 :: Encoder Word16
word16 = Stream . EWord16

-- | Encode a 'Word32' value.
{-# INLINE word32 #-}
word32 :: Encoder Word32
word32 = Stream . EWord32

-- | Encode a 'Word64' value.
{-# INLINE word64 #-}
word64 :: Encoder Word64
word64 = Stream . EWord64

-- | Encode an 'Int' value.
{-# INLINE int #-}
int :: Encoder Int
int = Stream . EInt

-- | Encode an 'Int8' value.
{-# INLINE int8 #-}
int8 :: Encoder Int8
int8 = Stream . EInt8

-- | Encode an 'Int16' value.
{-# INLINE int16 #-}
int16 :: Encoder Int16
int16 = Stream . EInt16

-- | Encode an 'Int32' value.
{-# INLINE int32 #-}
int32 :: Encoder Int32
int32 = Stream . EInt32

-- | Encode an 'Int64' value.
{-# INLINE int64 #-}
int64 :: Encoder Int64
int64 = Stream . EInt64

-- | Encode a 'Char' value.
{-# INLINE char #-}
char :: Encoder Char
char = Stream . EChar

-- | Encode a 'S.ByteString' value.
{-# INLINE byteString #-}
byteString :: Encoder S.ByteString
byteString = Stream . EByteString

{-
{-# INLINE builder #-}
builder :: Encoder B.Builder
builder = Stream . EBuilder
-}

-- Combinators
--------------


-- | Encode a list of values using the given 'Encoder' as follows.
--
-- We prefix the list with its length and encode its values in reverse order.
-- The reverse order slightly speeds-up decoding and does not really cost on
-- encoding.
-- {-# INLINE encodeList #-}
encodeList :: Encoder a -> Encoder [a]
encodeList f =
    go (0 :: Int) mempty
  where
    go !len acc []     = int len <> acc
    go !len acc (x:xs) = go (len + 1) (f x <> acc) xs

-- Alternatives:
--
-- encodeList f = (<> word8 0) . foldMap ((word8 1 <>) . f)
--
-- ^ Tagged encoding: works in a streaming fashion but requires more space and
-- is more expensive to decode.

-- encodeList = \f xs -> int (length xs) <> foldMap f xs
--
-- ^ Length-prefixing and standard-order encoding of the list elements. This
-- is slightly more expensive to decode as then the reversal has to be made
-- during decoding. It might nevertheless be preferred, as the primitive
-- streams of values become more readable.
