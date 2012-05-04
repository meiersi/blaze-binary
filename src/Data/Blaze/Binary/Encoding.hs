{-# LANGUAGE CPP, BangPatterns, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Blaze.Binary.Encoding
-- Copyright   : 2012, Simon Meier <iridcode@gmail.com>
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   :
-- Portability : portable
--
-- Binary encoding of values.
--
-----------------------------------------------------------------------------
module Data.Blaze.Binary.Encoding (

    -- * Streams of values to be encoded
      VStream
    , render
    , renderTagged
    , renderTextualUtf8

    -- ** Encoding combinators

    -- ** Construction
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
    
    , builder

    , (<>)

  ) where

import Prelude hiding (putChar)

import qualified Data.ByteString                                     as S
import qualified Data.ByteString.Char8                               as SC8
import qualified Data.ByteString.Lazy                                as L
import qualified Data.ByteString.Lazy.Builder                        as B
import qualified Data.ByteString.Lazy.Builder.ASCII                  as B
import qualified Data.ByteString.Lazy.Builder.Extras                 as B
import qualified Data.ByteString.Lazy.Builder.Internal               as B
import qualified Data.ByteString.Lazy.Builder.BasicEncoding          as E
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Internal as E
import           Data.Monoid
import           Data.Word
import           Data.Int
import           Foreign.Ptr

#if __GLASGOW_HASKELL__ < 704

infixr 6 <>

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

#endif

------------------------------------------------------------------------

-- | The representation for a stream of values to be serialized.
data VStreamRep =
       VChar           {-# UNPACK #-} !Char         VStreamRep
     | VWord           {-# UNPACK #-} !Word         VStreamRep
     | VWord8          {-# UNPACK #-} !Word8        VStreamRep
     | VWord16         {-# UNPACK #-} !Word16       VStreamRep
     | VWord32         {-# UNPACK #-} !Word32       VStreamRep
     | VWord64         {-# UNPACK #-} !Word64       VStreamRep
     | VInt            {-# UNPACK #-} !Int          VStreamRep
     | VInt8           {-# UNPACK #-} !Int8         VStreamRep
     | VInt16          {-# UNPACK #-} !Int16        VStreamRep
     | VInt32          {-# UNPACK #-} !Int32        VStreamRep
     | VInt64          {-# UNPACK #-} !Int64        VStreamRep
     | VFloat          {-# UNPACK #-} !Float        VStreamRep
     | VDouble         {-# UNPACK #-} !Double       VStreamRep
     | VInteger                       !Integer      VStreamRep
     | VByteString                    !S.ByteString VStreamRep
     | VBuilder                       !B.Builder    VStreamRep
     | VEmpty

-- | A stream of values to be encoded.
newtype VStream = VStream { toVStreamRep :: VStreamRep -> VStreamRep }

instance Monoid VStream where
  {-# INLINE mempty #-}
  mempty                 = VStream id
  {-# INLINE mappend #-}
  b1 `mappend` b2        = VStream (toVStreamRep b1 . toVStreamRep b2)
  {-# INLINE mconcat #-}
  mconcat                = foldr mappend mempty

-- | Binary encode a 'VStream' to a lazy bytestring 'B.Builder'.
render :: VStream -> B.Builder
render = renderWith
    (fe E.word8) (fe E.word16LE) (fe E.word32LE) (fe E.word64LE) (fe (fromIntegral E.>$< E.word64LE))
    (fe E.int8)  (fe E.int16LE)  (fe E.int32LE)  (fe E.int64LE)  (fe (fromIntegral E.>$< E.int64LE))
    E.charUtf8 (fe E.floatLE) (fe E.doubleLE)
    (error "render: integer: implement")
    B.byteString
    id
  where
    {-# INLINE fe #-}
    fe = E.fromF

-- | Binary encode a 'VStream' to a lazy bytestring 'B.Builder' using a tagged
-- format that allows to reconstruct the value stream.
renderTagged :: VStream -> L.ByteString
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

-- | Binary encode a 'VStream' to a lazy bytestring 'B.Builder'.
{-# INLINE renderWith #-}
renderWith :: E.BoundedEncoding Word8 -> E.BoundedEncoding Word16 -> E.BoundedEncoding Word32 -> E.BoundedEncoding Word64 -> E.BoundedEncoding Word
           -> E.BoundedEncoding Int8  -> E.BoundedEncoding Int16  -> E.BoundedEncoding Int32  -> E.BoundedEncoding Int64  -> E.BoundedEncoding Int
           -> E.BoundedEncoding Char
           -> E.BoundedEncoding Float -> E.BoundedEncoding Double
           -> (Integer -> B.Builder)
           -> (S.ByteString -> B.Builder)
           -> (B.Builder -> B.Builder)
           -> VStream
           -> B.Builder
renderWith w8 w16 w32 w64 w i8 i16 i32 i64 i c f d ibig bs b =
    -- take care that inlining is possible once all encodings are fixed
    \vs0 -> B.builder $ step (toVStreamRep vs0 VEmpty)
  where
    step vs1 k (B.BufferRange op0 ope0) = 
        go vs1 op0
      where
        go vs !op
          | op `plusPtr` bound <= ope0 = case vs of
              VEmpty            -> k (B.BufferRange op ope0)
              VWord8  x vs'     -> E.runB w8  x op >>= go vs'
              VWord16 x vs'     -> E.runB w16 x op >>= go vs'
              VWord32 x vs'     -> E.runB w32 x op >>= go vs'
              VWord64 x vs'     -> E.runB w64 x op >>= go vs'
              VWord   x vs'     -> E.runB w   x op >>= go vs'
              VInt8   x vs'     -> E.runB i8  x op >>= go vs'
              VInt16  x vs'     -> E.runB i16 x op >>= go vs'
              VInt32  x vs'     -> E.runB i32 x op >>= go vs'
              VInt64  x vs'     -> E.runB i64 x op >>= go vs'
              VInt    x vs'     -> E.runB i   x op >>= go vs'
              VChar   x vs'     -> E.runB c   x op >>= go vs'
              VFloat  x vs'     -> E.runB f   x op >>= go vs'
              VDouble x vs'     -> E.runB d   x op >>= go vs'
              VInteger x vs'    -> B.runBuilderWith (ibig x) (step vs' k) (B.BufferRange op ope0)
              VByteString x vs' -> B.runBuilderWith (bs x)   (step vs' k) (B.BufferRange op ope0)
              VBuilder x vs'    -> B.runBuilderWith (b x)    (step vs' k) (B.BufferRange op ope0)
          | otherwise = return $ B.bufferFull bound op (step vs k)

    bound = max' w8 $ max' w16 $ max' w32 $ max' w64 $ max' w $
            max' i8 $ max' i16 $ max' i32 $ max' i64 $ max' i $
            max' c  $ max' f $ E.sizeBound d

    {-# INLINE max' #-}
    max' e = max (E.sizeBound e)


renderTextualUtf8 :: VStream -> L.ByteString
renderTextualUtf8 vs0 =
    B.toLazyByteString $ go (toVStreamRep vs0 VEmpty)
  where
    go VEmpty                  = mempty
    go (VWord8 1 (VChar x vs)) = line "w8,c  1," (B.charUtf8 x) vs
    go (VWord8 1 (VWord x vs)) = line "w8,w  1," (B.wordDec x)  vs
    go (VWord8 1 (VInt  x vs)) = line "w8,i  1," (B.intDec x)   vs
    go (VInt l (VByteString x vs))           
      | l > 0                  = line "i,bs  " (B.intDec l <> B.char8 ',' <> B.byteStringHexFixed x) vs
    go (VWord8  x vs)          = line "w8    " (B.word8Dec x)  vs
    go (VWord16 x vs)          = line "w16   " (B.word16Dec x) vs
    go (VWord32 x vs)          = line "w32   " (B.word32Dec x) vs
    go (VWord64 x vs)          = line "w64   " (B.word64Dec x) vs
    go (VWord   x vs)          = line "w     " (B.wordDec x)   vs
    go (VInt8  x vs)           = line "i8    " (B.int8Dec x)  vs
    go (VInt16 x vs)           = line "i16   " (B.int16Dec x) vs
    go (VInt32 x vs)           = line "i32   " (B.int32Dec x) vs
    go (VInt64 x vs)           = line "i64   " (B.int64Dec x) vs
    go (VInt   x vs)           = line "i     " (B.intDec x)   vs
    go (VChar   x vs)          = line "c     " (B.charUtf8 x)  vs
    go (VFloat  x vs)          = line "f     " (B.floatDec x)  vs
    go (VDouble x vs)          = line "d     " (B.doubleDec x) vs
    go (VInteger x vs)         = line "I     " (B.integerDec x) vs
    go (VByteString x vs)      = line "bs    " (B.byteStringHexFixed x) vs
    go (VBuilder x vs)         = line "B     " (B.lazyByteStringHexFixed $ B.toLazyByteString x) vs

    line :: SC8.ByteString -> B.Builder -> VStreamRep -> B.Builder
    line pre b vs = B.byteStringCopy pre <> b <> B.char8 '\n' <> go vs


-- VStream construction
------------------------------

{-# INLINE float #-}
float :: Float -> VStream
float = VStream . VFloat

{-# INLINE double #-}
double :: Double -> VStream
double = VStream . VDouble

{-# INLINE integer #-}
integer :: Integer -> VStream
integer = VStream . VInteger

{-# INLINE word #-}
word :: Word -> VStream
word = VStream . VWord

{-# INLINE word8 #-}
word8 :: Word8 -> VStream
word8 = VStream . VWord8

{-# INLINE word16 #-}
word16 :: Word16 -> VStream
word16 = VStream . VWord16

{-# INLINE word32 #-}
word32 :: Word32 -> VStream
word32 = VStream . VWord32

{-# INLINE word64 #-}
word64 :: Word64 -> VStream
word64 = VStream . VWord64

{-# INLINE int #-}
int :: Int -> VStream
int = VStream . VInt

{-# INLINE int8 #-}
int8 :: Int8 -> VStream
int8 = VStream . VInt8

{-# INLINE int16 #-}
int16 :: Int16 -> VStream
int16 = VStream . VInt16

{-# INLINE int32 #-}
int32 :: Int32 -> VStream
int32 = VStream . VInt32

{-# INLINE int64 #-}
int64 :: Int64 -> VStream
int64 = VStream . VInt64

{-# INLINE char #-}
char :: Char -> VStream
char = VStream . VChar

{-# INLINE byteString #-}
byteString :: S.ByteString -> VStream
byteString = VStream . VByteString

{-# INLINE builder #-}
builder :: B.Builder -> VStream
builder = VStream . VBuilder


