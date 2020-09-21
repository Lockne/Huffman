{-# LANGUAGE DeriveFunctor #-}

module Weighted where

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
