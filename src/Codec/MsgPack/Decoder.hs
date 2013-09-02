{-# LANGUAGE MagicHash  #-}
{-# LANGUAGE Rank2Types #-}
-- | A specialized decoder for MsgPack.
module Codec.MsgPack.Decoder where

import           Control.Applicative

import qualified Data.ByteString as S
import qualified Data.Text       as T
import           Foreign
import           GHC.Prim

-- | The representation for a stream of values to be serialized.
data MsgPackValue
    = VArrayLen {-# UNPACK #-} !Word
    | VWord64   {-# UNPACK #-} !Word64
    | VInt64    {-# UNPACK #-} !Int64
    | VChar     {-# UNPACK #-} !Char
    | VBool                    !Bool
    | VFloat    {-# UNPACK #-} !Float
    | VDouble   {-# UNPACK #-} !Double
    | VMapLen   {-# UNPACK #-} !Word
    | VText                    !T.Text
    | VBinary                  !S.ByteString
    | VNil
    | VEnd
    deriving (Show)

data Decoder a = Decoder
    { runDecoder :: forall r. (a -> InStream r) -> InStream r }

data InStream a
    = GetValue (MsgPackValue -> InStream a)
    | Fail String
    | Done a


-- instances
------------

instance Functor Decoder where
    {-# INLINE fmap #-}
    fmap f = \d -> Decoder $ \k -> runDecoder d (k . f)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure = \x -> Decoder $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> Decoder $ \k ->
                        runDecoder df (\f -> runDecoder dx (\x -> k (f x)))

instance Monad Decoder where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> Decoder $ \k -> runDecoder dm (\m -> runDecoder (f m) k)

    {-# INLINE (>>) #-}
    (>>) = \dm dn -> Decoder $ \k -> runDecoder dm (\_ -> runDecoder dn k)

    fail msg = Decoder $ \_ -> Fail msg



-- Decoder construction
------------------------------

wrongType :: String -> MsgPackValue -> InStream a
wrongType expected v =
    Fail $ "expected " ++ expected ++ ", but got " ++ show v

primD :: (forall r. (a -> InStream r) -> MsgPackValue -> InStream r) -> Decoder a
primD f = Decoder $ \k -> GetValue $ \v -> f k v

{-# INLINE nil #-}
nil :: Decoder ()
nil = primD $ \k v -> case v of VNil -> k ()
                                _    -> wrongType "nil" v


{-# INLINE bool #-}
bool :: Decoder Bool
bool = primD $ \k v -> case v of VBool x -> k x
                                 _       -> wrongType "bool" v

{-# INLINE float #-}
float :: Decoder Float
float = primD $ \k v -> case v of VFloat x -> k x
                                  _        -> wrongType "float" v

{-# INLINE double #-}
double :: Decoder Double
double = primD $ \k v -> case v of VDouble x -> k x
                                   _         -> wrongType "double" v

{-
{-# INLINE word #-}
word :: Decoder Word
word =

{-# INLINE word8 #-}
word8 :: Decoder Word8
word8 =

{-# INLINE word16 #-}
word16 :: Decoder Word16
word16 =

{-# INLINE word32 #-}
word32 :: Decoder Word32
word32 =

{-# INLINE word64 #-}
word64 :: Decoder Word64
word64 =

{-# INLINE int #-}
int :: Decoder Int
int = primD . OInt64 . fromIntegral

{-# INLINE int8 #-}
int8 :: Decoder Int8
int8 = primD . OInt64 . fromIntegral

{-# INLINE int16 #-}
int16 :: Decoder Int16
int16 = primD . OInt64 . fromIntegral

{-# INLINE int32 #-}
int32 :: Decoder Int32
int32 = primD . OInt64 . fromIntegral

{-# INLINE int64 #-}
int64 :: Decoder Int64
int64 = primD . OInt64

{-# INLINE binary #-}
binary :: Decoder S.ByteString
binary = primD . OBinary

{-# INLINE char #-}
char :: Decoder Char
char = primD . OChar

{-# INLINE text #-}
text :: Decoder T.Text
text = primD . OText

{-# INLINE arrayLen #-}
arrayLen :: Decoder Int
arrayLen = primD . OArrayLen . fromIntegral

{-# INLINE mapLen #-}
mapLen :: Decoder Int
mapLen = primD . OMapLen . fromIntegral
-}
