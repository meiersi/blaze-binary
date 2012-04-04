{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures, UndecidableInstances #-}

import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import           Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8
import qualified Data.ByteString.Lazy as L
-- import qualified Data.Set as S
import           Numeric (showHex)

import           GHC.Generics

------------------------------------------------------------------------------
-- Counting the number of constructors of a datatype
------------------------------------------------------------------------------

class NumConstrs f where
    gNumConstrs :: f x -> Int

instance NumConstrs f => NumConstrs (M1 D c f) where
    gNumConstrs _ = gNumConstrs (undefined :: f x) 

instance NumConstrs (M1 C c f) where
    gNumConstrs _ = 1

instance (NumConstrs f, NumConstrs g) => NumConstrs (f :+: g) where
    gNumConstrs _ = 
        gNumConstrs (undefined :: f x) + gNumConstrs (undefined :: g x)

-- | Generic, compile time computation of the number of constructors of a
-- datatype.
numConstrs :: forall a. (Generic a, NumConstrs (Rep a)) => a -> Int
numConstrs _ = gNumConstrs (from (undefined :: a))


------------------------------------------------------------------------------
-- Generic Serialization
------------------------------------------------------------------------------

putIntHost :: Int -> Builder
putIntHost = fromInthost

putWord8Tag :: Int -> Builder
putWord8Tag = fromInt8 . fromIntegral

putWord16Tag :: Int -> Builder
putWord16Tag = fromInt16be . fromIntegral

fromTag :: Int -> Int -> Builder
fromTag n tag
  | n < 0xff   = putWord8Tag tag
  | n < 0xffff = putWord16Tag tag
  | otherwise  = error "fromTag: more than 0xfffff constructors"

-- The class duo responsible for serialization
----------------------------------------------

type Parser a = Maybe a

class GBinary f where
    gToBinary
        :: Int
        -- ^ Maximum number of constructors of the value being serialized
        -> Maybe Int  
        -- ^ Number of constructers to the left of the current parameter
        -- of the value being serialized
        -> f a 
        -- ^ The value to serialize
        -> Builder
        -- ^ Resulting sequence of bytes represented as a 'Builder'

    gFromBinary
        :: Maybe Int
        -- ^ Number of constructers to the left of the current parameter
        -- of the value being serialized
        -> Parser (f a)
        -- ^ Backup parser in case this one does not apply
        -> (Int -> Parser (f a))
        -- ^ Parser for final value

class Binary a where
    -- ^ Encode a value
    toBinary   :: a -> Builder
    fromBinary :: Parser a

    -- Generic default instance
    default toBinary :: (Generic a, NumConstrs (Rep a), GBinary (Rep a)) 
                     => a -> Builder
    toBinary x = gToBinary (numConstrs x) Nothing (from x)

    default fromBinary 
        :: (Generic a, NumConstrs (Rep a), GBinary (Rep a)) 
        => Parser a
    fromBinary = case numConstrs (undefined :: a) of 
      0 -> return (error "fromBinary: phantom type - impossible")
      1 -> to <$> gFromBinary Nothing undefined undefined 
      _ -> error "blah"
      
     

-- Instances for generic serialization
--------------------------------------

infixr 6 <>

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

instance Binary (V1 a) where
  toBinary   = mempty
  fromBinary = return (error "fromBinary V1: impossible")

instance Binary (U1 a) where
  toBinary    = mempty
  fromBinary = return U1

instance Binary a => Binary (K1 i f a) where
  toBinary   (K1 x) = toBinary x
  fromBinary        = K1 <$> fromBinary

instance Binary (f a) => Binary (M1 S c f a) where
  toBinary (M1 x) = toBinary x

instance Binary (f a) => Binary (M1 D c f a) where
  toBinary (M1 x) = toBinary x

{-
instance GBinary V1 where
  gToBinary _ _ _   = mempty
  gFromBinary _ _ _ = return (error "gFromBinary V1: impossible")

instance GBinary U1 where
  gToBinary _ _ _   = mempty
  gFromBinary _ _ _ = return U1

instance Binary a => GBinary (K1 i a) where
  gToBinary _ _ (K1 x) = toBinary x
  gFromBinary _ _ _    = K1 <$> fromBinary
-}

instance GBinary f => GBinary (M1 S c f) where
  gToBinary n _ (M1 x) = gToBinary n Nothing x

instance GBinary f => GBinary (M1 D c f) where
  gToBinary n _ (M1 x) = gToBinary n Nothing x

instance Binary (f a) => GBinary (M1 C c f) where
  gToBinary n Nothing    (M1 x) = toBinary x
  gToBinary n (Just tag) (M1 x) = fromTag n tag <> toBinary x
  -- gToBinary n Nothing    (M1 x) =                  toBinary n Nothing x
  -- gToBinary n (Just tag) (M1 x) = fromTag n tag <> gToBinary n Nothing x

  -- gFromBinary Nothing backup tag = M1 <$> gFromBinary Nothing backup tag
  -- gFromBinary (Just tag') backup tag 
    -- | tag' == tag = M1 <$> gFromBinary

instance (NumConstrs f, GBinary f, GBinary g) => GBinary (f :+: g) where
  gToBinary n off x = case x of
      L1 l -> gToBinary n (Just o) l
      R1 r -> gToBinary n (Just (o + (gNumConstrs (undefined :: f a)))) r
    where
      o = fromMaybe 0 off

instance (GBinary f, GBinary g) => GBinary (f :*: g) where
  gToBinary n _ (x :*: y) = gToBinary n Nothing x <> gToBinary n Nothing y


instance Binary Int where
    toBinary = putIntHost

instance Binary Char where
    toBinary = Utf8.fromChar

instance (Binary a, Binary b) => Binary (a, b) where
instance Binary a => Binary (Maybe a) where
instance (Binary a, Binary b) => Binary (Either a b) where
instance Binary a => Binary [a] where
instance Binary a => Binary (Expr a) where

------------------------------------------------------------------------------
-- Test Cases
------------------------------------------------------------------------------

data Expr a =
         Var a
       | Lit (Maybe Int)
       | Plus (Expr a) (Expr a)
       deriving( Eq, Ord, Generic )
 

testBinary :: Binary a => a -> IO ()
testBinary = putStrLn . concatMap (pad . (`showHex` "")) 
        . L.unpack . toLazyByteString . toBinary 
    where
      pad ""  = "00"
      pad [c] = '0' : c : []
      pad cs  = cs

main :: IO ()
main = testBinary $ 'a' -- (Plus (Var 'a') (Var 'b'))


e1 :: Expr String
e1 = Plus (Var "haskell") (Lit (Just 1))

------------------------------------------------------------------------------
-- Further Experiments
------------------------------------------------------------------------------

{-
data Tree b a = Leaf b a
              | Node (Tree (b, b) a) (Tree b (a, a))
            deriving (Generic)

-}


{-
----
-- Goal: gather all type names referenced by a type

class Types a where
   types :: a -> S.Set String -> S.Set String

   default types :: (Generic a, GTypes (Rep a)) 
                 => a -> S.Set String -> S.Set String
   types x = gtypes (from x)

instance Types Int where
   types _ = S.intoBinaryt "Int"

instance Types Char where
   types _ = S.intoBinaryt "Char"

class GTypes f where
   gtypes :: f p -> S.Set String -> S.Set String

instance (GTypes f, Datatype c) => GTypes (D1 c f) where
   gtypes x tys
     -- | ty `S.member` tys = tys
     | otherwise         = gtypes (unM1 x) (S.intoBinaryt ty tys)
     where
       ty = datatypeName x

instance GTypes f => GTypes (C1 c f) where
   gtypes _ = gtypes (undefined :: f a)

instance GTypes f => GTypes (S1 c f) where
   gtypes _ = gtypes (undefined :: f a)

instance Types a => GTypes (Rec0 a) where
   gtypes _ = id -- types (undefined :: a)

instance (GTypes f, GTypes g) => GTypes (f :+: g) where
    gtypes _ tys = gtypes (undefined :: f a) (gtypes (undefined :: g a) tys)

instance (GTypes f, GTypes g) => GTypes (f :*: g) where
    gtypes _ tys = gtypes (undefined :: f a) (gtypes (undefined :: g a) tys)

instance Types a => GTypes (Par0 a) where
    gtypes _ = types (undefined :: a)

instance GTypes V1 where
    gtypes _ tys = tys

instance GTypes U1 where
    gtypes _ tys = tys



instance Types a => Types (Maybe a) where
instance (Types a, Types b) => Types (Either a b) where
instance Types a => Types [a] where

instance Types a => Types (Expr a) where

getTypes :: Types a => a -> S.Set String
getTypes x = types x S.empty

-}
