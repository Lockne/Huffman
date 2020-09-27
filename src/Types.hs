{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Binary
import GHC.Generics
import Data.Foldable

-- | PrefixTree : Datatype to implement a Huffman encoding tree.
data PrefixTree a
  = PTLeaf a
  | PTNode (PrefixTree a) (PrefixTree a)
  deriving (Eq, Show, Generic)

instance Binary a => Binary (PrefixTree a)

data Weighted a =
  WPair
    { _wWeight :: Int
    , _wItem :: a
    }
  deriving (Show, Functor)

instance Eq (Weighted a) where
  WPair w1 _ == WPair w2 _ = w1 == w2

instance Ord (Weighted a) where
  compare (WPair w1 _) (WPair w2 _) = compare w1 w2

-- | WeightedPT : type synonym that associates a PrefixTree with
--                a weight.
type WeightedPT a = Weighted (PrefixTree a)

data Direction
  = DLeft
  | DRight
  deriving (Eq, Show)

type Encoding = [Direction]

data SkewHeap a
  = SEmpty
  | SNode (SkewHeap a) a (SkewHeap a)
  deriving (Show, Eq, Foldable)

-- | PQueue is a newtype wrapper for SkewHeap.
newtype PQueue a =
  PQ (SkewHeap a)
  deriving (Show, Foldable)
