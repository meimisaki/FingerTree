{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude hiding (splitAt)

import Control.Applicative
import Control.Monad

import Data.FingerTree
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Traversable

import Test.QuickCheck
import Test.QuickCheck.Monadic

import GHC.Exts (fromList)

instance Arbitrary a => Arbitrary (Seq a) where
    arbitrary = fromList `liftM` arbitrary

instance Show (a -> b) where
    show _ = "(->)"

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
    toList ((fromList :: [a] -> Seq a) xs) == xs

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
