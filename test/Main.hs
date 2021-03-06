{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude hiding (splitAt)

import Control.Applicative
import Control.Monad

import Data.FingerTree
import Data.Functor.Compose
import Data.Functor.Identity
import qualified Data.List as L
import Data.Maybe
import Data.Traversable

import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified GHC.Exts as Exts

instance (Ord a, Arbitrary a) => Arbitrary (OrdSeq a) where
    arbitrary = Exts.fromList `liftM` arbitrary

instance (Ord a, Arbitrary a) => Arbitrary (PQueue a) where
    arbitrary = Exts.fromList `liftM` arbitrary

instance Arbitrary a => Arbitrary (Seq a) where
    arbitrary = Exts.fromList `liftM` arbitrary

instance Show (a -> b) where
    show _ = "(->)"

prop_OrdSeqMinViewIdentity :: Ord a => OrdSeq a -> Bool
prop_OrdSeqMinViewIdentity xs = case minView xs of
    Nothing -> True
    Just (x, ys) -> insert x ys == deleteMin (insert x xs)

prop_OrdSeqMaxViewIdentity :: Ord a => OrdSeq a -> Bool
prop_OrdSeqMaxViewIdentity xs = case maxView xs of
    Nothing -> True
    Just (ys, x) -> insert x ys == deleteMax (insert x xs)

prop_OrdSeqMonoidIdentity :: Ord a => OrdSeq a -> Bool
prop_OrdSeqMonoidIdentity xs =
    mempty <> xs == xs

prop_OrdSeqMonoidAssociativity :: Ord a => OrdSeq a -> OrdSeq a -> OrdSeq a -> Bool
prop_OrdSeqMonoidAssociativity xs ys zs =
    xs <> (ys <> zs) == (xs <> ys) <> zs

prop_OrdSeqMonoidCommutativity :: Ord a => OrdSeq a -> OrdSeq a -> Bool
prop_OrdSeqMonoidCommutativity xs ys =
    xs <> ys == ys <> xs

prop_OrdSeqIsList :: Ord a => OrdSeq a -> Bool
prop_OrdSeqIsList xs =
    Exts.fromList (toList xs) == xs

prop_OrdSeqPartition :: Ord a => a -> OrdSeq a -> Bool
prop_OrdSeqPartition x xs =
    l <> r == xs && all (< x) l && all (>= x) r
    where (l, r) = partition x xs

prop_OrdSeqInsert :: Ord a => a -> OrdSeq a -> Bool
prop_OrdSeqInsert x xs =
    toList (insert x xs) == L.insert x (toList xs)

prop_OrdSeqDeleteAll :: Ord a => a -> OrdSeq a -> Bool
prop_OrdSeqDeleteAll x xs =
    toList (deleteAll x xs) == [a | a <- toList xs, a /= x]

prop_OrdSeqMerge :: Ord a => OrdSeq a -> OrdSeq a -> Bool
prop_OrdSeqMerge xs ys =
    toList (merge xs ys) == mergeList (toList xs) (toList ys)
    where mergeList [] ys = ys
          mergeList xs [] = xs
          mergeList (x:xs) (y:ys)
            | x < y = x:mergeList xs (y:ys)
            | otherwise = y:mergeList (x:xs) ys

prop_OrdSeqIntersect :: Ord a => OrdSeq a -> OrdSeq a -> Bool
prop_OrdSeqIntersect xs ys =
    toList (intersect xs ys) == intersectList (toList xs) (toList ys)
    where intersectList [] _ = []
          intersectList _ [] = []
          intersectList (x:xs) (y:ys)
            | x < y = intersectList xs (y:ys)
            | x > y = intersectList (x:xs) ys
            | otherwise = x:intersectList (drop xs) (drop ys)
                where drop = dropWhile (== x)

prop_OrdSeqOrdering :: Ord a => OrdSeq a -> Bool
prop_OrdSeqOrdering xs =
    map (fst . fromJust) (takeWhile isJust ys) == toList xs
    where ys = iterate (>>= minView . snd) (minView xs)

prop_PQueueMaxViewIdentity :: Ord a => PQueue a -> Bool
prop_PQueueMaxViewIdentity xs = case maxView xs of
    Nothing -> True
    Just (ys, x) -> insert x ys == deleteMax (insert x xs)

prop_PQueueMonoidIdentity :: Ord a => PQueue a -> Bool
prop_PQueueMonoidIdentity xs =
    mempty <> xs == xs

prop_PQueueMonoidAssociativity :: Ord a => PQueue a -> PQueue a -> PQueue a -> Bool
prop_PQueueMonoidAssociativity xs ys zs =
    xs <> (ys <> zs) == (xs <> ys) <> zs

prop_PQueueMonoidCommutativity :: Ord a => PQueue a -> PQueue a -> Bool
prop_PQueueMonoidCommutativity xs ys =
    xs <> ys == ys <> xs

prop_PQueueIsList :: Ord a => PQueue a -> Bool
prop_PQueueIsList xs =
    Exts.fromList (Exts.toList xs) == xs

prop_PQueueOrdering :: Ord a => PQueue a -> Bool
prop_PQueueOrdering xs =
    map (snd . fromJust) (takeWhile isJust ys) == Exts.toList xs
    where ys = iterate (>>= maxView . fst) (maxView xs)

prop_SeqFunctorIdentity :: Eq a => Seq a -> Bool
prop_SeqFunctorIdentity xs =
    (id <$> xs) == id xs

prop_SeqFunctorComposition :: (Eq a, Num a) => (a -> a) -> (a -> a) -> Seq a -> Bool
prop_SeqFunctorComposition f g xs =
    (f . g <$> xs) == (fmap f . fmap g) xs

prop_SeqApplicativeIdentity :: Eq a => Seq a -> Bool
prop_SeqApplicativeIdentity xs =
    (pure id <*> xs) == xs

prop_SeqApplicativeComposition :: (Eq a, Num a) => (a -> a) -> (a -> a) -> Seq a -> Bool
prop_SeqApplicativeComposition (pure -> f) (pure -> g) xs =
    (pure (.) <*> f <*> g <*> xs) == (f <*> (g <*> xs))

prop_SeqApplicativeHomomorphism :: (Eq a, Num a) => (a -> a) -> a -> Bool
prop_SeqApplicativeHomomorphism f x =
    (pure f <*> pure x) == (pure :: a -> Seq a) (f x)

prop_SeqApplicativeInterchange :: (Eq a, Num a) => (a -> a) -> a -> Bool
prop_SeqApplicativeInterchange f x =
    (pure f <*> pure x) == (pure ($ x) <*> (pure :: a -> Seq a) f)

prop_SeqMonadLeftIdentity :: (Eq a, Num a) => (a -> Seq a) -> a -> Bool
prop_SeqMonadLeftIdentity f x =
    ((return :: a -> Seq a) x >>= f) == f x

prop_SeqMonadRightIdentity :: Eq a => Seq a -> Bool
prop_SeqMonadRightIdentity xs =
    (xs >>= return) == xs

prop_SeqMonadAssociativity :: Eq a => (a -> Seq a) -> (a -> Seq a) -> Seq a -> Bool
prop_SeqMonadAssociativity f g xs =
    ((xs >>= f) >>= g) == (xs >>= (\x -> f x >>= g))

prop_SeqTraversableNaturality :: Eq a => Seq a -> Bool
prop_SeqTraversableNaturality xs =
    t (traverse f xs) == traverse (t . f) xs
    where t (Just x) = [x]
          f = Just

prop_SeqTraversableIdentity :: Eq a => Seq a -> Bool
prop_SeqTraversableIdentity xs =
    traverse Identity xs == Identity xs

prop_SeqTraversableComposition :: Eq a => Seq a -> Bool
prop_SeqTraversableComposition xs =
    traverse (Compose . fmap g . f) xs == Compose (fmap (traverse g) (traverse f xs))
    where f = Just
          g x = [x]

prop_SeqDequeLeftIdentity :: Eq a => a -> Seq a -> Bool
prop_SeqDequeLeftIdentity x xs =
    viewL (x <| xs) == Just (x, xs)

prop_SeqDequeRightIdentity :: Eq a => Seq a -> a -> Bool
prop_SeqDequeRightIdentity xs x =
    viewR (xs |> x) == Just (xs, x)

prop_SeqMonoidIdentity :: Eq a => Seq a -> Bool
prop_SeqMonoidIdentity xs =
    (mempty <> xs == xs) && (xs <> mempty == xs)

prop_SeqMonoidAssociativity :: Eq a => Seq a -> Seq a -> Seq a -> Bool
prop_SeqMonoidAssociativity xs ys zs =
    xs <> (ys <> zs) == (xs <> ys) <> zs

prop_SeqIsList :: Eq a => [a] -> Bool
prop_SeqIsList xs =
    toList ((Exts.fromList :: [a] -> Seq a) xs) == xs

prop_SeqSplit :: Eq a => Seq a -> Property
prop_SeqSplit xs = n > 0 ==> (monadic' $ do
    i <- run (choose (0, n - 1))
    let (as, bs) = splitAt i xs
    assert (as <> bs == xs))
    where n = length xs

prop_SeqAccess :: Eq a => Seq a -> Bool
prop_SeqAccess xs =
    map (xs !) [0..length xs - 1] == toList xs

return []
main = $forAllProperties quickCheckResult >>= \x ->
    when (not x) (error "failed")
