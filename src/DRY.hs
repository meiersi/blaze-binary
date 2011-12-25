-- | Author: Simon Meier <iridcode@gmail.com>
--
-- Experiment on using partial isomorphisms to define 'Binary' instances
-- using the well-known tagged format. If the required speed is achievable,
-- then this might provide an interesting tradeoff between flexiblity and ease
-- of use.
--
-- Obviously, we also wan't to try out the new generic stuff.
-- The advantage of this format here is that it can be extended to support
-- safe upgrades  (a-la safecopy), as the tags are managed explicitly.
-- Implicit, tag management is no good idea in that respect. Partial
-- isomorphisms are also rather nice in that respect. An old constructor
-- becomes:
--
--   oldConstr = Iso (const Nothing, \(constr_args) -> newCosntr constr_args)

module DonRepeatYourselfInBinary where

import Control.Applicative
import Control.Monad
import Control.Isomorphism.Partial (inverse)
import Control.Isomorphism.Partial.Unsafe
import Control.Isomorphism.Partial.Constructors (nil, cons)

import Data.Maybe
import Data.Monoid
import Data.Binary (Binary(..), Get)
import Data.Binary.Builder

data TaggedIso tag alpha beta =
       TaggedIso (alpha -> Maybe (tag, beta))
                 -- conversion from abstract to tagged concrete
                 (tag -> Maybe (beta -> alpha))
                 -- conversion from tagged concrete to abstract

withTag t (Iso to from) = TaggedIso 
    (fmap (fmap ((,) t)) to)
    (\t' -> if t == t' then Just (fromJust . from) else Nothing)

newtype Rep a = Rep { unRep :: (a -> Builder, Get a) }

newtype TaggedRep a = TaggedRep 
   { unTaggedRep :: (a -> Maybe Builder, Int -> Maybe (Get a)) }

instance Monoid (TaggedRep a) where
   mempty = TaggedRep (const Nothing, const Nothing)

   TaggedRep (b1, g1) `mappend` TaggedRep (b2, g2) = 
       TaggedRep ( \x -> b1 x `mappend` b2 x 
                 , \x -> g1 x `mplus` g2 x)

class Bin a where
    rep :: Rep a

build :: Bin a => a -> Builder
build = fst $ unRep rep

getit :: Bin a => Get a
getit = snd $ unRep rep

tag :: Bin b => Int -> Iso b a -> TaggedRep a
tag t (Iso to from) = TaggedRep
  ( \x -> ((build t `mappend`) . build) <$> from x
  , \t' -> if t == t' then Just ((fromJust . to) <$> getit) else Nothing)

instance Bin () where
    rep = Rep (const mempty, return ())

instance (Bin a, Bin b) => Bin (a, b) where
    rep = Rep ( \(x,y) -> build x `mappend` build y
              , (,) <$> getit <*> getit)

fromTaggedRep :: TaggedRep a -> Rep a
fromTaggedRep (TaggedRep (enc, dec)) = 
  Rep (fromJust . enc, get >>= (fromJust . dec))

fromIso :: Bin b => Iso a b -> Rep a
fromIso (Iso from to) = Rep
  ( build . fromJust . from
  , (fromJust . to) <$> getit)

enumBool :: Iso Bool Int
enumBool = Iso ( \b -> Just $ if b then 1 else 0)
               ( \i -> case i of 0 -> Just False; 1 -> Just True; _ -> Nothing)

instance Bin Int where
    rep = Rep (putWordhost . fromIntegral, get)

instance Bin Bool where
    rep = fromIso enumBool

instance Bin a => Bin [a] where
    rep = fromTaggedRep $ tag 0 nil `mappend` 
                          tag 1 cons
{-
    mkRep $ tag 1 nil <>
            tag 2 cons 
-}

