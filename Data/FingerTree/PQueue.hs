module Data.FingerTree.PQueue
( PQueue (..)
, top
, push
, pop
) where

import Data.FingerTree.Internal
import Data.FingerTree.Lazy

newtype PQueue a = PQueue { getPQueue :: FingerTree (Prio a) (Elem a) }

instance Ord a => Monoid (PQueue a) where
    mempty = PQueue mempty
    PQueue xs `mappend` PQueue ys = PQueue (xs <> ys)

top :: Ord a => PQueue a -> Maybe a
top (PQueue xs) = if isEmpty xs
    then Nothing
    else let Prio x = (measure xs) in Just x

push :: Ord a => a -> PQueue a -> PQueue a
push x (PQueue xs) = PQueue (Elem x <| xs)

pop :: Ord a => PQueue a -> Maybe (PQueue a)
pop (PQueue xs) = if isEmpty xs
    then Nothing
    else Just (PQueue (l <> r))
    where Split l _ r = splitTree (measure xs <=) mempty xs
