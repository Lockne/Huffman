{-# LANGUAGE DeriveFoldable #-}

module PQueue
  ( PQueue
  , emptyPQ
  , insertPQ
  , popPQ
  , sizePQ
  ) where

import Data.Foldable

-- | (Internal implementation) SkewHeap
data SkewHeap a
  = SEmpty
  | SNode (SkewHeap a) a (SkewHeap a)
  deriving (Show, Eq, Foldable)

makeSH :: a -> SkewHeap a
makeSH x = SNode SEmpty x SEmpty

popSH :: Ord a => SkewHeap a -> (Maybe a, SkewHeap a)
popSH SEmpty = (Nothing, SEmpty)
popSH (SNode h1 r h2) = (Just r, mergeSH h1 h2)

mergeSH :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeSH SEmpty h = h
mergeSH h SEmpty = h
mergeSH hA@(SNode lA xA rA) hB@(SNode lB xB rB)
  | xA < xB = SNode (mergeSH rA hB) xA lA
  | otherwise = SNode (mergeSH rB hA) xB lB

-- | PQueue is a newtype wrapper for SkewHeap.
--   We will export only the functions below
--   and only to these will the user have access.
newtype PQueue a =
  PQ (SkewHeap a)
  deriving (Show, Foldable)

emptyPQ :: PQueue a
emptyPQ = PQ SEmpty

insertPQ :: Ord a => a -> PQueue a -> PQueue a
insertPQ x (PQ h) = PQ $ mergeSH h (makeSH x)

popPQ :: Ord a => PQueue a -> (Maybe a, PQueue a)
popPQ (PQ h) = (poppedVal, PQ h')
  where
    (poppedVal, h') = popSH h

sizePQ :: PQueue a -> Int
sizePQ = length . toList
