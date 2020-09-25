{-# LANGUAGE DeriveGeneric #-}

module PrefixTree
  ( WeightedPT
  , makeWPT
  , mergeWPT
  , PrefixTree
  ) where

import GHC.Generics

-- | Goals :
--      Understand GHC.Generics
import Weighted

-- | PrefixTree : Datatype to implement a Huffman encoding tree.
data PrefixTree a
  = PTLeaf a
  | PTNode (PrefixTree a) (PrefixTree a)
  deriving (Eq, Show, Generic)

-- | makePT : takes a value and generates a PrefixTree from it.
makePT :: a -> PrefixTree a
makePT = PTLeaf

-- | mergePT : takes two PrefixTrees and merges them into one.
mergePT :: PrefixTree a -> PrefixTree a -> PrefixTree a
mergePT = PTNode

-- | WeightedPT : type synonym that associates a PrefixTree with
--                a weight.
type WeightedPT a = Weighted (PrefixTree a)

-- | makeWPT : takes a weight, a value and generates a weighted
--             PrefixTree.
makeWPT :: Int -> a -> WeightedPT a
makeWPT w = WPair w . makePT

-- | mergeWPT : takes two Weighted PrefixTrees and merges them.
mergeWPT :: WeightedPT a -> WeightedPT a -> WeightedPT a
mergeWPT (WPair w1 pt1) (WPair w2 pt2) = WPair (w1 + w2) (mergePT pt1 pt2)
