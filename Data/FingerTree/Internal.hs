{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Data.FingerTree.Internal
( module Data.Foldable
, module Data.Monoid
, Elem (..)
, Prio (..)
, Key (..)
, Measured (..)
, Deque (..)
, foldMapDeque
, Unfoldable (..)
, MinView (..)
, MaxView (..)
) where

import Prelude hiding (any)

import Control.Arrow

import Data.Foldable
import Data.Maybe
import Data.Monoid

newtype Elem a = Elem { getElem :: a }
    deriving (Eq, Ord)

data Prio a = MInf | Prio a
    deriving (Eq, Ord)

instance Ord a => Monoid (Prio a) where
    mempty = MInf
    MInf `mappend` p = p
    p `mappend` MInf = p
    Prio m `mappend` Prio n = Prio (max m n)

data Key a = NoKey | Key a
    deriving (Eq, Ord)

instance Monoid (Key a) where
    mempty = NoKey
    k `mappend` NoKey = k
    _ `mappend` k = k

class Monoid v => Measured a v where
    measure :: a -> v

instance (Measured (Elem a) v1, Measured (Elem a) v2) => Measured (Elem a) (v1, v2) where
    measure = measure &&& measure

instance Ord a => Measured (Elem a) (Prio a) where
    measure (Elem x) = Prio x

instance Measured (Elem a) (Key a) where
    measure (Elem x) = Key x

class Foldable t => Deque t a where
    infixr 5 <|
    (<|) :: a -> t a -> t a
    infixl 5 |>
    (|>) :: t a -> a -> t a
    viewL :: Monad m => t a -> m (a, t a)
    viewR :: Monad m => t a -> m (t a, a)
    headL :: t a -> a
    tailL :: t a -> t a
    headR :: t a -> a
    tailR :: t a -> t a
    viewL (null -> True) = fail ""
    viewL xs = return (headL xs, tailL xs)
    viewR (null -> True) = fail ""
    viewR xs = return (tailR xs, headR xs)
    headL = fst . fromJust . viewL
    tailL = snd . fromJust . viewL
    headR = snd . fromJust . viewR
    tailR = fst . fromJust . viewR
    {-# MINIMAL (<|), (|>), (viewL | headL, tailL), (viewR | headR, tailR) #-}

foldMapDeque :: (Deque t a, Monoid m) => (a -> m) -> t a -> m
foldMapDeque f (null -> True) = mempty
foldMapDeque f xs = f (headL xs) <> foldMapDeque f (tailL xs)

class Unfoldable t a where
    insert :: a -> t a -> t a

class Ord a => MinView t a where
    minView :: Monad m => t a -> m (a, t a)
    minElem :: t a -> a
    deleteMin :: t a -> t a
    minElem = fst . fromJust . minView
    deleteMin = snd . fromJust . minView
    {-# MINIMAL minView #-}

class Ord a => MaxView t a where
    maxView :: Monad m => t a -> m (t a, a)
    maxElem :: t a -> a
    deleteMax :: t a -> t a
    maxElem = snd . fromJust . maxView
    deleteMax = fst . fromJust . maxView
    {-# MINIMAL maxView #-}
