{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.FingerTree.OrdSeq
( OrdSeq (..)
, Unfoldable (..)
, MinView (..)
, MaxView (..)
, partition
, deleteAll
, merge
, intersect
) where

import Control.Arrow

import Data.FingerTree.Internal
import Data.FingerTree.Lazy

import Data.List (sort)

import qualified GHC.Exts as Exts

newtype OrdSeq a = OrdSeq { getOrdSeq :: FingerTree (Key a) (Elem a) }

instance Show a => Show (OrdSeq a) where
    show = show . toList

instance Foldable OrdSeq where
    foldMap f = foldMap (f . getElem) . getOrdSeq

instance Ord a => Unfoldable OrdSeq a where
    insert x (OrdSeq xs) = OrdSeq (l <> (Elem x <| r))
        where (l, r) = split (>= Key x) xs

instance Ord a => MinView OrdSeq a where
    minView = fmap (getElem *** OrdSeq) . viewL . getOrdSeq

instance Ord a => MaxView OrdSeq a where
    maxView = fmap (OrdSeq *** getElem) . viewR . getOrdSeq

instance Ord a => Monoid (OrdSeq a) where
    mempty = OrdSeq mempty
    mappend = merge

instance Eq a => Eq (OrdSeq a) where
    xs == ys = toList xs == toList ys

instance Ord a => Ord (OrdSeq a) where
    compare xs ys = compare (toList xs) (toList ys)

instance Ord a => Exts.IsList (OrdSeq a) where
    type Item (OrdSeq a) = a
    fromList = OrdSeq . fromFoldable . map Elem . sort
    toList = toList

partition :: Ord a => a -> OrdSeq a -> (OrdSeq a, OrdSeq a)
partition x (OrdSeq xs) = (OrdSeq l, OrdSeq r)
    where (l, r) = split (>= Key x) xs

deleteAll :: Ord a => a -> OrdSeq a -> OrdSeq a
deleteAll x (OrdSeq xs) = OrdSeq (l <> r)
    where (l, m) = split (>= Key x) xs
          (_, r) = split (> Key x) m

merge :: Ord a => OrdSeq a -> OrdSeq a -> OrdSeq a
merge (OrdSeq xs) (OrdSeq ys) = OrdSeq (merge' xs ys)
    where merge' xs (null -> True) = xs
          merge' xs (viewL -> Just (y, ys)) = l <> (y <| merge' ys r)
            where (l, r) = split (> measure y) xs

intersect :: Ord a => OrdSeq a -> OrdSeq a -> OrdSeq a
intersect (OrdSeq xs) (OrdSeq ys) = OrdSeq (intersect' xs ys)
    where intersect' xs (null -> True) = mempty
          intersect' xs (viewL -> Just (y, ys)) = if null l then zs else y <| zs
            where zs = intersect' ys r
                  (_, m) = split (>= k) xs
                  (l, r) = split (> k) m
                  k = measure y
