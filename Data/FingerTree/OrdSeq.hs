{-# LANGUAGE TypeFamilies #-}

module Data.FingerTree.OrdSeq
( OrdSeq (..)
, partition
, insert
, deleteAll
, merge
, intersect
) where

import Data.FingerTree.Internal
import Data.FingerTree.Lazy

import Data.List (sort)

import qualified GHC.Exts as Exts

newtype OrdSeq a = OrdSeq { getOrdSeq :: FingerTree (Key a) (Elem a) }

instance Show a => Show (OrdSeq a) where
    show = show . toList

instance Foldable OrdSeq where
    foldMap f = foldMap (f . getElem) . getOrdSeq

instance Ord a => Monoid (OrdSeq a) where
    mempty = OrdSeq mempty
    mappend = merge

instance Ord a => Exts.IsList (OrdSeq a) where
    type Item (OrdSeq a) = a
    fromList = OrdSeq . fromFoldable . map Elem . sort
    toList = toList

partition :: Ord a => a -> OrdSeq a -> (OrdSeq a, OrdSeq a)
partition x (OrdSeq xs) = (OrdSeq l, OrdSeq r)
    where (l, r) = split (>= Key x) xs

insert :: Ord a => a -> OrdSeq a -> OrdSeq a
insert x (OrdSeq xs) = OrdSeq (l <> (Elem x <| r))
    where (l, r) = split (>= Key x) xs

deleteAll :: Ord a => a -> OrdSeq a -> OrdSeq a
deleteAll x (OrdSeq xs) = OrdSeq (l <> r)
    where (l, m) = split (>= Key x) xs
          (_, r) = split (> Key x) m

merge :: Ord a => OrdSeq a -> OrdSeq a -> OrdSeq a
merge (OrdSeq xs) (OrdSeq ys) = OrdSeq (merge' xs ys)
    where merge' xs ys = case viewL ys of
            Nothing -> xs
            Just (z, zs) -> l <> (z <| merge' zs r)
                where (l, r) = split (> measure z) xs

intersect :: Ord a => OrdSeq a -> OrdSeq a -> OrdSeq a
intersect (OrdSeq xs) (OrdSeq ys) = OrdSeq (intersect' xs ys)
    where intersect' xs ys = case viewL ys of
            Nothing -> mempty
            Just (z, zs) -> l <> intersect' zs r
                where (_, m) = split (>= k) xs
                      (l, r) = split (> k) m
                      k = measure z
