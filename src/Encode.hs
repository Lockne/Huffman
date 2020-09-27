{-# LANGUAGE DeriveGeneric #-}

module Encode (encodePTree) where

import qualified Data.Map.Lazy as M
import Types (Direction(..), Encoding(..), PrefixTree(..))

ptTable :: Ord a => PrefixTree a -> M.Map a Encoding
ptTable pt = go pt []
  where
    go (PTLeaf x) enc = x `M.singleton` reverse enc
    go (PTNode pt1 pt2) enc = go pt1 (DLeft : enc) <> go pt2 (DRight : enc)

lookupPTTable :: Ord a => a -> M.Map a Encoding -> Maybe Encoding
lookupPTTable = M.lookup

encodePTree :: Ord a => PrefixTree a -> [a] -> Maybe Encoding
encodePTree pt xs = concat <$> sequence (map (flip lookupPTTable tb) xs)
  where
    tb = ptTable pt
