{-# LANGUAGE DeriveGeneric #-}

module PrefixTree
  ( makeWPT
  , mergeWPT
  ) where

import Types (PrefixTree(..), Weighted(..), WeightedPT(..))

-- | makePT : takes a value and generates a PrefixTree from it.
makePT :: a -> PrefixTree a
makePT = PTLeaf

-- | mergePT : takes two PrefixTrees and merges them into one.
mergePT :: PrefixTree a -> PrefixTree a -> PrefixTree a
mergePT = PTNode

-- | makeWPT : takes a weight, a value and generates a weighted
--             PrefixTree.
makeWPT :: Int -> a -> WeightedPT a
makeWPT w = WPair w . makePT

-- | mergeWPT : takes two Weighted PrefixTrees and merges them.
mergeWPT :: WeightedPT a -> WeightedPT a -> WeightedPT a
mergeWPT (WPair w1 pt1) (WPair w2 pt2) = WPair (w1 + w2) (mergePT pt1 pt2)
