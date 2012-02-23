{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures, UndecidableInstances #-}

import           Data.Maybe
import           Data.Monoid
import           Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8
import qualified Data.ByteString.Lazy as L
-- import qualified Data.Set as S
import           Numeric (showHex)

import           GHC.Generics


data Expr a =
         Var a
       | Lit (Maybe Int)
       | Plus (Expr a) (Expr a)
       deriving( Eq, Ord, Generic )
 
class NConstrs f where
  gnconstrs :: f x -> Int

instance NConstrs f => NConstrs (M1 D c f) where
  gnconstrs _ = gnconstrs (undefined :: f x) 

instance NConstrs (M1 C c f) where
  gnconstrs _ = 1

instance (NConstrs f, NConstrs g) => NConstrs (f :+: g) where
  gnconstrs _ = 
      gnconstrs (undefined :: f x) + gnconstrs (undefined :: g x)

putIntHost :: Int -> Builder
putIntHost = fromInthost

putWord8Tag :: Int -> Builder
putWord8Tag = fromInt8 . fromIntegral

putWord16Tag :: Int -> Builder
putWord16Tag = fromInt16be . fromIntegral

nconstrs :: forall a. (Generic a, NConstrs (Rep a)) => a -> Int
nconstrs _ = gnconstrs (from (undefined :: a))

class GSer f where
  gser :: Int -> Maybe Int -> f () -> Builder

instance GSer U1 where
  gser _ _ _ = mempty

instance Ser c => GSer (K1 i c) where
  gser _ _ (K1 x) = ser x

instance GSer f => GSer (M1 S c f) where
  gser n _ (M1 x) = gser n Nothing x

instance GSer f => GSer (M1 D c f) where
  gser n _ (M1 x) = gser n Nothing x

instance GSer f => GSer (M1 C c f) where
  gser n Nothing    (M1 x) = gser n Nothing x
  gser n (Just tag) (M1 x)
    | n <= 0xff   = putWord8Tag  tag `mappend` gser n Nothing x
    | n <= 0xffff = putWord16Tag tag `mappend` gser n Nothing x
    | otherwise   = error "GSer: max. 0xffff constructors supported."

instance (NConstrs f, GSer f, GSer g) => GSer (f :+: g) where
  gser n off x = case x of
      L1 l -> gser n (Just o) l
      R1 r -> gser n (Just (o + (gnconstrs (undefined :: f ())))) r
    where
      o = fromMaybe 0 off

instance (GSer f, GSer g) => GSer (f :*: g) where
  gser n _ (x :*: y) = gser n Nothing x `mappend` gser n Nothing y

class Ser a where
    ser :: a -> Builder

    default ser :: (Generic a, NConstrs (Rep a), GSer (Rep a)) => a -> Builder
    ser x = gser (nconstrs x) Nothing (from x)

instance Ser Int where
    ser = putIntHost

instance Ser Char where
    ser = Utf8.fromChar

instance (Ser a, Ser b) => Ser (a, b) where
instance Ser a => Ser (Maybe a) where
instance Ser a => Ser [a] where
instance Ser a => Ser (Expr a) where

testSer :: Ser a => a -> IO ()
testSer = putStrLn . concatMap (pad . (`showHex` "")) 
        . L.unpack . toLazyByteString . ser 
    where
      pad ""  = "00"
      pad [c] = '0' : c : []
      pad cs  = cs

main :: IO ()
main = testSer $ 'a' -- (Plus (Var 'a') (Var 'b'))


e1 :: Expr String
e1 = Plus (Var "haskell") (Lit (Just 1))

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
   types _ = S.insert "Int"

instance Types Char where
   types _ = S.insert "Char"

class GTypes f where
   gtypes :: f p -> S.Set String -> S.Set String

instance (GTypes f, Datatype c) => GTypes (D1 c f) where
   gtypes x tys
     -- | ty `S.member` tys = tys
     | otherwise         = gtypes (unM1 x) (S.insert ty tys)
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
