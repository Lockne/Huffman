module Huffman where

import Control.Monad.Trans.State.Lazy
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import PQueue
import PrefixTree
import Types

-- | FreqTable : This takes a string, generates a table with
--               key values as the elements of the string,
--               and table values as the number of times each
--               character appears in the string
type FreqTable a = M.Map a Int

-- | listToFreq : Takes a string, and converts each character into a key.
--                Against each key, the value 1 is inserted. If a character repeats
--                itself in the string, the value 1 is added to the existing value
--                for every repetition.
listToFreq :: Ord a => [a] -> FreqTable a
listToFreq = foldr f M.empty
  where
    f x m = M.insertWith (+) x 1 m

-- | listQueue : Creates Huffman leaves out of the elements of a string,
--               with associated weights, and inserts them into a Priority Queue.
listQueue :: Ord a => [a] -> PQueue (WeightedPT a)
listQueue = M.foldrWithKey f emptyPQ . listToFreq
  where
    f k v pq = insertPQ (makeWPT v k) pq

buildTree :: State (PQueue (WeightedPT a)) (Maybe (PrefixTree a))
buildTree = do
  t1 <- state popPQ
  case t1 of
    Nothing -> return Nothing
    Just t1 -> do
      t2 <- state popPQ
      case t2 of
        Nothing -> return $ Just $ _wItem t1
        Just t2 -> do
          let combined = mergeWPT t1 t2
          modify (insertPQ combined)
          buildTree

runBuildTree :: Ord a => [a] -> (Maybe (PrefixTree a))
runBuildTree xs = evalState (buildTree) (listQueue xs)
