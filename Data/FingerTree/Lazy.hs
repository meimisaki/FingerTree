{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Data.FingerTree.Lazy
( FingerTree
, Measured (..)
, fromFoldable
, Split (..)
, splitTree
, split
) where

import Prelude hiding (foldl, foldr)

import Data.FingerTree.Internal

data Node v a = Node2 v a a | Node3 v a a a
    deriving (Show, Foldable)

node2 :: Measured a v => a -> a -> Node v a
node2 a b = Node2 v a b
    where v = measure [a, b]

node3 :: Measured a v => a -> a -> a -> Node v a
node3 a b c = Node3 v a b c
    where v = measure [a, b, c]

data FingerTree v a
    = Empty
    | Single a
    | Deep v [a] (FingerTree v (Node v a)) [a]
    deriving (Show, Foldable)

deep :: Measured a v => [a] -> FingerTree v (Node v a) -> [a] -> FingerTree v a
deep pr m sf = Deep v pr m sf
    where v = measure pr <> measure m <> measure sf

instance Monoid v => Measured (Node v a) v where
    measure (Node2 v _ _) = v
    measure (Node3 v _ _ _) = v

instance Measured a v => Measured [a] v where
    measure = mconcat . map measure

instance Measured a v => Measured (FingerTree v a) v where
    measure Empty = mempty
    measure (Single a) = measure a
    measure (Deep v _ _ _) = v

deepL :: Measured a v => [a] -> FingerTree v (Node v a) -> [a] -> FingerTree v a
deepL [] (viewL -> Nothing) sf = fromFoldable sf
deepL [] (viewL -> Just (pr, m)) sf = deep (toList pr) m sf
deepL pr m sf = deep pr m sf

deepR :: Measured a v => [a] -> FingerTree v (Node v a) -> [a] -> FingerTree v a
deepR pr (viewR -> Nothing) [] = fromFoldable pr
deepR pr (viewR -> Just (m, sf)) [] = deep pr m (toList sf)
deepR pr m sf = deep pr m sf

instance Measured a v => Deque (FingerTree v) a where
    a <| Empty = Single a
    a <| Single b = deep [a] Empty [b]
    a <| Deep _ [b, c, d, e] m sf = deep [a, b] (node3 c d e <| m) sf
    a <| Deep _ pr m sf = deep ([a] ++ pr) m sf
    Empty |> a = Single a
    Single b |> a = deep [b] Empty [a]
    Deep _ pr m [e, d, c, b] |> a = deep pr (m |> node3 e d c) [b, a]
    Deep _ pr m sf |> a = deep pr m (sf ++ [a])
    viewL Empty = fail ""
    viewL (Single x) = return (x, Empty)
    viewL (Deep _ pr m sf) = return (head pr, deepL (tail pr) m sf)
    viewR Empty = fail ""
    viewR (Single x) = return (Empty, x)
    viewR (Deep _ pr m sf) = return (deepR pr m (init sf), last sf)

fromFoldable :: (Foldable t, Measured a v) => t a -> FingerTree v a
fromFoldable = foldr (<|) Empty

instance Measured a v => Monoid (FingerTree v a) where
    mempty = Empty
    xs `mappend` ys = app3 xs [] ys

app3 :: Measured a v => FingerTree v a -> [a] -> FingerTree v a -> FingerTree v a
app3 Empty ts xs = foldr (<|) xs ts
app3 xs ts Empty = foldl (|>) xs ts
app3 (Single x) ts xs = x <| foldr (<|) xs ts
app3 xs ts (Single x) = foldl (|>) xs ts |> x
app3 (Deep _ pr1 m1 sf1) ts (Deep _ pr2 m2 sf2) =
    deep pr1 (app3 m1 (nodes (sf1 ++ ts ++ pr2)) m2) sf2
    where nodes [a, b] = [node2 a b]
          nodes [a, b, c] = [node3 a b c]
          nodes [a, b, c, d] = [node2 a b, node2 c d]
          nodes (a:b:c:xs) = node3 a b c:nodes xs

data Split f a = Split (f a) a (f a)

splitList :: Measured a v => (v -> Bool) -> v -> [a] -> Split [] a
splitList _ _ [x] = Split [] x []
splitList p i (x:xs)
    | p i' = Split [] x xs
    | otherwise = let Split l a r = splitList p i' xs in Split (x:l) a r
    where i' = i <> measure x

splitTree :: Measured a v => (v -> Bool) -> v -> FingerTree v a -> Split (FingerTree v) a
splitTree _ _ (Single x) = Split Empty x Empty
splitTree p i (Deep _ pr m sf)
    | p vpr = let Split l x r = splitList p i pr
              in Split (fromFoldable l) x (deepL r m sf)
    | p vm = let Split ml xs mr = splitTree p vpr m
                 Split l x r = splitList p (vpr <> measure ml) (toList xs)
             in Split (deepR pr ml l) x (deepL r mr sf)
    | otherwise = let Split l x r = splitList p vm sf
                  in Split (deepR pr m l) x (fromFoldable r)
    where vpr = i <> measure pr
          vm = vpr <> measure m

split :: Measured a v => (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
split _ Empty = (Empty, Empty)
split p xs
    | p (measure xs) = (l, x <| r)
    | otherwise = (xs, Empty)
    where Split l x r = splitTree p mempty xs
