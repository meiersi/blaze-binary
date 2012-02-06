{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures, UndecidableInstances #-}

import Data.Maybe
import GHC.Generics

data Expr a =
         Var a
       | Int (Maybe Int)
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

