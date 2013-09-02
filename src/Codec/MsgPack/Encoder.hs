{-# LANGUAGE CPP, BangPatterns, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Codec.MsgPack.Encoder
-- Copyright   : 2013, Simon Meier <iridcode@gmail.com>
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   :
-- Portability : portable
--
-- MsgPack encoding support.
--
-----------------------------------------------------------------------------

{- An improved binary format for the 'binary' library -
 ------------------------------------------------------

   *  MsgPack: a binary format with a similar structure as JSON

   * can be parsed without schema/type information

      data Object
          = ONil
          | OBool Bool
          | OInteger Int
          | OFloat Float
          | ODouble Double
          | OBinary ByteString
          | OText   Text
          | OArray [Object]
          | OMap [(Object, Object)]


   * compact representation; e.g., integers

        positive fixnum stores 7-bit positive integer
        +--------+
        |0XXXXXXX|
        +--------+

        * 0XXXXXXX is 8-bit unsigned integer

        uint 8 stores a 8-bit unsigned integer
        +--------+--------+
        |  0xcc  |ZZZZZZZZ|
        +--------+--------+

        uint 16 stores a 16-bit big-endian unsigned integer
        +--------+--------+--------+
        |  0xcd  |ZZZZZZZZ|ZZZZZZZZ|
        +--------+--------+--------+

        uint 32 stores a 32-bit big-endian unsigned integer
        +--------+--------+--------+--------+--------+
        |  0xce  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ
        +--------+--------+--------+--------+--------+

        uint 64 stores a 64-bit big-endian unsigned integer
        +--------+--------+--------+--------+--------+--------+--------+--------+--------+
        |  0xcf  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
        +--------+--------+--------+--------+--------+--------+--------+--------+--------+


   * Goal: use a msg-pack based binary format for our standard binary encoding
           and get for free

            - schema-less parsing (important for debugging network requests)
            - language interoperability
            - good small-message/small-value performance
            - machine-independence

   * using some clever implementation tricks, we can do this even faster than
     the most recent 'cereal' or 'binary' encoders.

      => let's look at two of them now.

-}





module Codec.MsgPack.Encoder (

      Encoder

    -- * Streams of values to be encoded
    , OutStream

    , toBuilder

      -- * Primitive output stream constructores
    , nil
    , bool
    , float
    , double
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
    , char
    , binary
    , text
    , arrayLen
    , mapLen
    ) where

import Prelude hiding (putChar)

import           Data.Bits
import           Data.Char                                           (ord)
import qualified Data.ByteString                                     as S
-- import qualified Data.ByteString.Char8                               as SC8
-- import qualified Data.ByteString.Lazy                                as L
import qualified Data.ByteString.Builder                        as B
-- import qualified Data.ByteString.Lazy.Builder.ASCII                  as B
-- import qualified Data.ByteString.Lazy.Builder.Extras                 as B
import qualified Data.ByteString.Builder.Internal               as BI
import qualified Data.ByteString.Builder.Prim.Internal         as PI
import qualified Data.ByteString.Builder.Prim                  as P
import           Data.ByteString.Builder.Prim  ( (>$<), (>*<), condB )
-- import qualified Data.ByteString.Lazy.Builder.BasicEncoder.Internal as E
import           Data.Monoid
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
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
data OutStreamRep
    = OArrayLen {-# UNPACK #-} !Word         OutStreamRep
    | OWord64   {-# UNPACK #-} !Word64       OutStreamRep
    | OInt64    {-# UNPACK #-} !Int64        OutStreamRep
    | OChar     {-# UNPACK #-} !Char         OutStreamRep
    | OFalse                                 OutStreamRep
    | OTrue                                  OutStreamRep
    | OFloat    {-# UNPACK #-} !Float        OutStreamRep
    | ODouble   {-# UNPACK #-} !Double       OutStreamRep
    | OMapLen   {-# UNPACK #-} !Word         OutStreamRep
    | OText                    !T.Text       OutStreamRep
    | OBinary                  !S.ByteString OutStreamRep
    | ONil                                   OutStreamRep
    | OEnd

type Encoder t = t -> OutStream

-- | A stream of values to be encoded.
newtype OutStream = OutStream { unOutStream :: OutStreamRep -> OutStreamRep }

instance Monoid OutStream where
  {-# INLINE mempty #-}
  mempty                 = OutStream id
  {-# INLINE mappend #-}
  b1 `mappend` b2        = OutStream (unOutStream b1 . unOutStream b2)
  {-# INLINE mconcat #-}
  mconcat                = foldr mappend mempty



-- | Convert a message-pack output stream to a bytestring 'B.Builder'.
toBuilder :: OutStream -> B.Builder
toBuilder =
    \vs0 -> BI.builder $ step (unOutStream vs0 OEnd)
  where
    step vs1 k (BI.BufferRange op0 ope0) =
        go vs1 op0
      where
        go vs !op
          | op `plusPtr` bound <= ope0 = case vs of
              OWord64   x vs' -> PI.runB word64MP   x op >>= go vs'
              OInt64    x vs' -> PI.runB int64MP    x op >>= go vs'
              ONil        vs' -> PI.runB nilMP ()     op >>= go vs'
              OFalse      vs' -> PI.runB falseMP ()   op >>= go vs'
              OTrue       vs' -> PI.runB trueMP ()    op >>= go vs'
              OFloat    x vs' -> PI.runB floatMP    x op >>= go vs'
              ODouble   x vs' -> PI.runB doubleMP   x op >>= go vs'
              OArrayLen x vs' -> PI.runB arrayLenMP x op >>= go vs'
              OMapLen   x vs' -> PI.runB mapLenMP   x op >>= go vs'
              OChar     x vs' -> PI.runB charMP     x op >>= go vs'
              OText     x vs' -> BI.runBuilderWith (textMP x)   (step vs' k) (BI.BufferRange op ope0)
              OBinary   x vs' -> BI.runBuilderWith (binaryMP x) (step vs' k) (BI.BufferRange op ope0)
              OEnd            -> k (BI.BufferRange op ope0)
          | otherwise = return $ BI.bufferFull bound op (step vs k)

    bound = max' word64MP $ max' int64MP $ max' nilMP $
            max' falseMP $ max' trueMP $ max' floatMP $ max' doubleMP $
            max' arrayLenMP $ max' mapLenMP $ PI.sizeBound charMP

    {-# INLINE max' #-}
    max' e = max (PI.sizeBound e)

{-
nil:
+--------+
|  0xc0  |
+--------+

-}

nilMP :: P.BoundedPrim ()
nilMP = fixed1 $ const 0xc0 >$< P.word8


{-
false:
+--------+
|  0xc2  |
+--------+
-}

falseMP :: P.BoundedPrim ()
falseMP = fixed1 $ const 0xc2 >$< P.word8

{-
true:
+--------+
|  0xc3  |
+--------+
-}

trueMP :: P.BoundedPrim ()
trueMP = fixed1 $ const 0xc3 >$< P.word8


{-
positive fixnum stores 7-bit positive integer
+--------+
|0XXXXXXX|
+--------+

* 0XXXXXXX is 8-bit unsigned integer

uint 8 stores a 8-bit unsigned integer
+--------+--------+
|  0xcc  |ZZZZZZZZ|
+--------+--------+

uint 16 stores a 16-bit big-endian unsigned integer
+--------+--------+--------+
|  0xcd  |ZZZZZZZZ|ZZZZZZZZ|
+--------+--------+--------+

uint 32 stores a 32-bit big-endian unsigned integer
+--------+--------+--------+--------+--------+
|  0xce  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ
+--------+--------+--------+--------+--------+

uint 64 stores a 64-bit big-endian unsigned integer
+--------+--------+--------+--------+--------+--------+--------+--------+--------+
|  0xcf  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
+--------+--------+--------+--------+--------+--------+--------+--------+--------+

-}

fixed1 :: P.FixedPrim a -> P.BoundedPrim a
fixed1 = P.liftFixedToBounded

preFixed1 :: Word8 -> P.FixedPrim a -> P.BoundedPrim a
preFixed1 header p = (\x -> (header, x)) >$< fixed1 (P.word8 >*< p)

{-# INLINE word64MP #-}
word64MP :: P.BoundedPrim Word64
word64MP =
    condB (<= 0x7f)       (   fixed1      (fromIntegral >$< P.word8   )) $
    condB (<= 0xff)       (preFixed1 0xcc (fromIntegral >$< P.word8   )) $
    condB (<= 0xffff)     (preFixed1 0xcd (fromIntegral >$< P.word16BE)) $
    condB (<= 0xffffffff) (preFixed1 0xce (fromIntegral >$< P.word32BE)) $
                          (preFixed1 0xcf                   P.word64BE )
{-# INLINE int64MP #-}
int64MP :: P.BoundedPrim Int64
int64MP =
    condB (>=  0)          (               (fromIntegral >$< word64MP  )) $
    condB (>= -32)         (   fixed1      (fromIntegral >$< P.word8   )) $
    condB (>= -0xff)       (preFixed1 0xd0 (fromIntegral >$< P.word8   )) $
    condB (>= -0xffff)     (preFixed1 0xd1 (fromIntegral >$< P.word16BE)) $
    condB (>= -0xffffffff) (preFixed1 0xd2 (fromIntegral >$< P.word32BE)) $
                           (preFixed1 0xd3 (fromIntegral >$< P.word64BE))

{-
negative fixnum stores 5-bit negative integer
+--------+
|111YYYYY|
+--------+

* 111YYYYY is 8-bit signed integer

int 8 stores a 8-bit signed integer
+--------+--------+
|  0xd0  |ZZZZZZZZ|
+--------+--------+

int 16 stores a 16-bit big-endian signed integer
+--------+--------+--------+
|  0xd1  |ZZZZZZZZ|ZZZZZZZZ|
+--------+--------+--------+

int 32 stores a 32-bit big-endian signed integer
+--------+--------+--------+--------+--------+
|  0xd2  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
+--------+--------+--------+--------+--------+

int 64 stores a 64-bit big-endian signed integer
+--------+--------+--------+--------+--------+--------+--------+--------+--------+
|  0xd3  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
+--------+--------+--------+--------+--------+--------+--------+--------+--------+
-}


{-# INLINE floatMP #-}
floatMP :: P.BoundedPrim Float
floatMP = preFixed1 0xca P.floatBE

{-# INLINE doubleMP #-}
doubleMP :: P.BoundedPrim Double
doubleMP = preFixed1 0xcb P.doubleBE

{-
float 32 stores a floating point number in IEEE 754 single precision floating point number format:
+--------+--------+--------+--------+--------+
|  0xca  |XXXXXXXX|XXXXXXXX|XXXXXXXX|XXXXXXXX
+--------+--------+--------+--------+--------+

float 64 stores a floating point number in IEEE 754 double precision floating point number format:
+--------+--------+--------+--------+--------+--------+--------+--------+--------+
|  0xcb  |YYYYYYYY|YYYYYYYY|YYYYYYYY|YYYYYYYY|YYYYYYYY|YYYYYYYY|YYYYYYYY|YYYYYYYY|
+--------+--------+--------+--------+--------+--------+--------+--------+--------+
-}

-- | A specialized version of message-pack encoding for single chars.
charMP :: P.BoundedPrim Char
charMP = (ord >$<) $
    condB (<= 0x7f)
      ( preFixed1 0xa1 $ fromIntegral >$< P.word8
      ) $
    condB (<= 0x07ff)
      ( (\x -> ( fromIntegral $ (x `shiftR` 6) + 0xC0
               , fromIntegral $ (x .&. 0x3F)   + 0x80
               )
        ) >$< (preFixed1 0xa2 $ P.word8 >*< P.word8)
      ) $
    condB (<= 0xffff)
      ( (\x -> ( fromIntegral $ (x `shiftR` 12) + 0xE0
               , ( fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
                 , fromIntegral $ (x .&. 0x3F) + 0x80
               ) )
        ) >$< (preFixed1 0xa3 $ P.word8 >*< (P.word8 >*< P.word8))
      ) $
      ( (\x -> ( fromIntegral $ (x `shiftR` 18) + 0xF0
               , ( fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
                 , ( fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
                   , fromIntegral $ (x .&. 0x3F) + 0x80
               ) ) )
        ) >$< (preFixed1 0xa4 $ P.word8 >*< (P.word8 >*< (P.word8 >*< P.word8)))
      )
{-
fixstr stores a byte array whose length is upto 31 bytes:
+--------+========+
|101XXXXX|  data  |
+--------+========+
-}

textMP :: T.Text -> B.Builder
textMP t =
    P.primBounded textLenMP (fromIntegral $ S.length bs) <> B.byteString bs
  where
    bs  = T.encodeUtf8 t

textLenMP :: P.BoundedPrim Word
textLenMP =
    condB (< 32    ) ((\l -> 0xa0 .|. fromIntegral l) >$< fixed1 P.word8) $
    condB (< 0xff  ) (fromIntegral >$< preFixed1 0xd9 P.word8) $
    condB (< 0xffff) (fromIntegral >$< preFixed1 0xda P.word16BE) $
                     (fromIntegral >$< preFixed1 0xdb P.word32BE)


{-
fixstr stores a byte array whose length is upto 31 bytes:
+--------+========+
|101XXXXX|  data  |
+--------+========+

str 8 stores a byte array whose length is upto (2^8)-1 bytes:
+--------+--------+========+
|  0xd9  |YYYYYYYY|  data  |
+--------+--------+========+

str 16 stores a byte array whose length is upto (2^16)-1 bytes:
+--------+--------+--------+========+
|  0xda  |ZZZZZZZZ|ZZZZZZZZ|  data  |
+--------+--------+--------+========+

str 32 stores a byte array whose length is upto (2^32)-1 bytes:
+--------+--------+--------+--------+--------+========+
|  0xdb  |AAAAAAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA|  data  |
+--------+--------+--------+--------+--------+========+
-}

binaryMP :: S.ByteString -> B.Builder
binaryMP bs =
    P.primBounded binaryLenMP (fromIntegral $ S.length bs) <> B.byteString bs

binaryLenMP :: P.BoundedPrim Word
binaryLenMP =
    condB (< 0xff  ) (fromIntegral >$< preFixed1 0xc4 P.word8) $
    condB (< 0xffff) (fromIntegral >$< preFixed1 0xc5 P.word16BE) $
                     (fromIntegral >$< preFixed1 0xc6 P.word32BE)



{-
bin 8 stores a byte array whose length is upto (2^8)-1 bytes:
+--------+--------+========+
|  0xc4  |XXXXXXXX|  data  |
+--------+--------+========+

bin 16 stores a byte array whose length is upto (2^16)-1 bytes:
+--------+--------+--------+========+
|  0xc5  |YYYYYYYY|YYYYYYYY|  data  |
+--------+--------+--------+========+

bin 32 stores a byte array whose length is upto (2^32)-1 bytes:
+--------+--------+--------+--------+--------+========+
|  0xc6  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|  data  |
+--------+--------+--------+--------+--------+========+
-}

arrayLenMP :: P.BoundedPrim Word
arrayLenMP =
    condB (< 16    ) ((\l -> 0xb0 .|. fromIntegral l) >$< fixed1 P.word8) $
    condB (< 0xffff) (fromIntegral >$< preFixed1 0xdc P.word16BE) $
                     (fromIntegral >$< preFixed1 0xdd P.word32BE)

{-
fixarray stores an array whose length is upto 15 elements:
+--------+~~~~~~~~~~~~~~~~~+
|1001XXXX|    N objects    |
+--------+~~~~~~~~~~~~~~~~~+

array 16 stores an array whose length is upto (2^16)-1 elements:
+--------+--------+--------+~~~~~~~~~~~~~~~~~+
|  0xdc  |YYYYYYYY|YYYYYYYY|    N objects    |
+--------+--------+--------+~~~~~~~~~~~~~~~~~+

array 32 stores an array whose length is upto (2^32)-1 elements:
+--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
|  0xdd  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|    N objects    |
+--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+

where
* XXXX is a 4-bit unsigned integer which represents N
* YYYYYYYY_YYYYYYYY is a 16-bit big-endian unsigned integer which represents N
* ZZZZZZZZ_ZZZZZZZZ_ZZZZZZZZ_ZZZZZZZZ is a 32-bit big-endian unsigned integer which represents N
    N is the size of a array
-}

mapLenMP :: P.BoundedPrim Word
mapLenMP =
    condB (< 16    ) ((\l -> 0x70 .|. fromIntegral l) >$< fixed1 P.word8) $
    condB (< 0xffff) (fromIntegral >$< preFixed1 0xde P.word16BE) $
                     (fromIntegral >$< preFixed1 0xdf P.word32BE)

{-
fixmap stores a map whose length is upto 15 elements
+--------+~~~~~~~~~~~~~~~~~+
|1000XXXX|   N*2 objects   |
+--------+~~~~~~~~~~~~~~~~~+

map 16 stores a map whose length is upto (2^16)-1 elements
+--------+--------+--------+~~~~~~~~~~~~~~~~~+
|  0xde  |YYYYYYYY|YYYYYYYY|   N*2 objects   |
+--------+--------+--------+~~~~~~~~~~~~~~~~~+

map 32 stores a map whose length is upto (2^32)-1 elements
+--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
|  0xdf  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|   N*2 objects   |
+--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+

where
* XXXX is a 4-bit unsigned integer which represents N
* YYYYYYYY_YYYYYYYY is a 16-bit big-endian unsigned integer which represents N
* ZZZZZZZZ_ZZZZZZZZ_ZZZZZZZZ_ZZZZZZZZ is a 32-bit big-endian unsigned integer which represents N
* N is the size of a map
* odd elements in objects are keys of a map
* the next element of a key is its associated value
-}

-- OutStream construction
------------------------------

{-# INLINE nil #-}
nil :: OutStream
nil = OutStream ONil

{-# INLINE false #-}
false :: OutStream
false = OutStream OFalse

{-# INLINE true #-}
true :: OutStream
true = OutStream OTrue

{-# INLINE bool #-}
bool :: Encoder Bool
bool = \x -> (if x then true else false)

{-# INLINE float #-}
float :: Encoder Float
float = OutStream . OFloat

{-# INLINE double #-}
double :: Encoder Double
double = OutStream . ODouble

{-# INLINE word #-}
word :: Encoder Word
word = OutStream . OWord64 . fromIntegral

{-# INLINE word8 #-}
word8 :: Encoder Word8
word8 = OutStream . OWord64 . fromIntegral

{-# INLINE word16 #-}
word16 :: Encoder Word16
word16 = OutStream . OWord64 . fromIntegral

{-# INLINE word32 #-}
word32 :: Encoder Word32
word32 = OutStream . OWord64 . fromIntegral

{-# INLINE word64 #-}
word64 :: Encoder Word64
word64 = OutStream . OWord64

{-# INLINE int #-}
int :: Encoder Int
int = OutStream . OInt64 . fromIntegral

{-# INLINE int8 #-}
int8 :: Encoder Int8
int8 = OutStream . OInt64 . fromIntegral

{-# INLINE int16 #-}
int16 :: Encoder Int16
int16 = OutStream . OInt64 . fromIntegral

{-# INLINE int32 #-}
int32 :: Encoder Int32
int32 = OutStream . OInt64 . fromIntegral

{-# INLINE int64 #-}
int64 :: Encoder Int64
int64 = OutStream . OInt64

{-# INLINE binary #-}
binary :: Encoder S.ByteString
binary = OutStream . OBinary

{-# INLINE char #-}
char :: Encoder Char
char = OutStream . OChar

{-# INLINE text #-}
text :: Encoder T.Text
text = OutStream . OText

{-# INLINE arrayLen #-}
arrayLen :: Encoder Int
arrayLen = OutStream . OArrayLen . fromIntegral

{-# INLINE mapLen #-}
mapLen :: Encoder Int
mapLen = OutStream . OMapLen . fromIntegral
