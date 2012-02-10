{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures, UndecidableInstances #-}

import Data.Maybe
import qualified Data.Set as S
import GHC.Generics

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


nconstrs :: forall a. (Generic a, NConstrs (Rep a)) => a -> Int
nconstrs _ = gnconstrs (from (undefined :: a))

class GSer f where
  gser :: Maybe Int -> f () -> String

instance GSer U1 where
  gser _ _ = ""

instance Ser c => GSer (K1 i c) where
  gser _ (K1 x) = ser x

instance GSer f => GSer (M1 S c f) where
  gser _ (M1 x) = gser Nothing x

instance GSer f => GSer (M1 D c f) where
  gser _ (M1 x) = gser Nothing x

instance GSer f => GSer (M1 C c f) where
  gser Nothing    (M1 x) = gser Nothing x
  gser (Just tag) (M1 x) = show tag ++ ":" ++ gser Nothing x

instance (NConstrs f, GSer f, GSer g) => GSer (f :+: g) where
  gser offset x = case x of
      L1 l -> gser (Just o) l
      R1 r -> gser (Just $ o + (gnconstrs (undefined :: f ()))) r
    where
      o = fromMaybe 0 offset

instance (GSer f, GSer g) => GSer (f :*: g) where
  gser _ (x :*: y) = gser Nothing x ++ gser Nothing y

class Ser a where
    ser :: a -> String

    default ser :: (Generic a, GSer (Rep a)) => a -> String
    ser x = gser Nothing (from x)

instance Ser Int where
    ser = show

instance Ser Char where
    ser = show

instance Ser a => Ser (Maybe a) where
instance Ser a => Ser (Expr a) where


main :: IO ()
main = putStrLn $ ser (Plus (Var 'a') (Var 'b'))

data Tree b a = Leaf b a
              | Node (Tree (b, b) a) (Tree b (a, a))
            deriving (Generic)

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

