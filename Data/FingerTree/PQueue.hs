{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FingerTree.PQueue
( PQueue (..)
, Unfoldable (..)
, MinView (..)
, MaxView (..)
) where

import Data.FingerTree.Internal
import Data.FingerTree.Lazy

import Data.List (sort)

import qualified GHC.Exts as Exts

newtype PQueue a = PQueue { getPQueue :: FingerTree (Prio a) (Elem a) }

instance (Ord a, Show a) => Show (PQueue a) where
    show = show . Exts.toList

instance Ord a => Unfoldable PQueue a where
    insert x (PQueue xs) = PQueue (Elem x <| xs)

instance Ord a => MaxView PQueue a where
    maxView (PQueue xs)
        | null xs = fail ""
        | otherwise = return (PQueue (l <> r), x)
        where Split l _ r = splitTree (Prio x <=) mempty xs
              Prio x = measure xs

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
