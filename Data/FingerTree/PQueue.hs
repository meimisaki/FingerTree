{-# LANGUAGE TypeFamilies #-}

module Data.FingerTree.PQueue
( PQueue (..)
, top
, push
, pop
) where

import Data.FingerTree.Internal
import Data.FingerTree.Lazy

import Data.List (sort)

import qualified GHC.Exts as Exts

newtype PQueue a = PQueue { getPQueue :: FingerTree (Prio a) (Elem a) }

instance (Ord a, Show a) => Show (PQueue a) where
    show = show . Exts.toList

instance Ord a => Monoid (PQueue a) where
    mempty = PQueue mempty
    PQueue xs `mappend` PQueue ys = PQueue (xs <> ys)

instance Ord a => Eq (PQueue a) where
    xs == ys = Exts.toList xs == Exts.toList ys

instance Ord a => Ord (PQueue a) where
    compare xs ys = compare (Exts.toList xs) (Exts.toList ys)

instance Ord a => Exts.IsList (PQueue a) where
    type Item (PQueue a) = a
    fromList = PQueue . fromFoldable . map Elem
    toList = reverse . sort . map getElem . toList . getPQueue

top :: Ord a => PQueue a -> Maybe a
top (PQueue xs) = if null xs
    then Nothing
    else let Prio x = (measure xs) in Just x

push :: Ord a => a -> PQueue a -> PQueue a
push x (PQueue xs) = PQueue (Elem x <| xs)

pop :: Ord a => PQueue a -> Maybe (PQueue a)
pop (PQueue xs) = if null xs
    then Nothing
    else Just (PQueue (l <> r))
    where Split l _ r = splitTree (measure xs <=) mempty xs
