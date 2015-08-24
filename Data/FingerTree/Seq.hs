{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FingerTree.Seq
( Seq (..)
, Deque (..)
, splitAt
, (!)
) where

import Prelude hiding (splitAt)

import Control.Applicative
import Control.Arrow
import Control.Monad

import Data.FingerTree.Internal
import Data.FingerTree.Lazy

import Data.Traversable

import qualified GHC.Exts as Exts

newtype Size = Size { getSize :: Int }
    deriving (Eq, Ord)

instance Monoid Size where
    mempty = Size 0
    Size m `mappend` Size n = Size (m + n)

instance Measured (Elem a) Size where
    measure _ = Size 1

newtype Seq a = Seq { getSeq :: FingerTree Size (Elem a) }

instance Show a => Show (Seq a) where
    show = show . toList

instance Functor Seq where
    fmap f = Exts.fromList . fmap f . toList

instance Applicative Seq where
    pure = return
    (<*>) = ap

instance Monad Seq where
    return = (<| mempty)
    (>>=) = flip foldMap
    fail _ = mempty

instance Foldable Seq where
    foldMap f = foldMap (f . getElem) . getSeq
    length (Seq xs) = getSize (measure xs)

instance Traversable Seq where
    traverse f xs = Exts.fromList <$> traverse f (toList xs)

instance Deque Seq a where
    x <| Seq xs = Seq (Elem x <| xs)
    Seq xs |> x = Seq (xs |> Elem x)
    viewL = fmap (getElem *** Seq) . viewL . getSeq
    viewR = fmap (Seq *** getElem) . viewR . getSeq

instance Monoid (Seq a) where
    mempty = Seq mempty
    Seq xs `mappend` Seq ys = Seq (xs <> ys)

instance Eq a => Eq (Seq a) where
    xs == ys = toList xs == toList ys

instance Ord a => Ord (Seq a) where
    compare xs ys = compare (toList xs) (toList ys)

instance Exts.IsList (Seq a) where
    type Item (Seq a) = a
    fromList = Seq . fromFoldable . map Elem
    toList = toList

splitAt :: Int -> Seq a -> (Seq a, Seq a)
splitAt i (Seq xs) = (Seq l, Seq r)
    where (l, r) = split (Size i <) xs

(!) :: Seq a -> Int -> a
Seq xs ! i = getElem x
    where Split _ x _ = splitTree (Size i <) mempty xs
