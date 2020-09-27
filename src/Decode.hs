{-# LANGUAGE LambdaCase #-}

module Decode (decodePTree) where

import Types (Direction(..), Encoding(..), PrefixTree(..))
import Data.List

decodePT :: PrefixTree a -> Encoding -> Maybe (a, Encoding)
decodePT (PTLeaf x) ds = Just (x, ds)
decodePT (PTNode pt1 pt2) (d:ds) =
  case d of
    DLeft -> decodePT pt1 ds
    DRight -> decodePT pt2 ds
decodePT (PTNode _ _) [] = Nothing

decodePTree :: PrefixTree a -> Encoding -> [a]
decodePTree pt = unfoldr (decodePT pt)
