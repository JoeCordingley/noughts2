module Bag (Bag, one, addAll, fromFoldable, size) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Sum(..))
import Data.Foldable (fold)

type Bag a = Map a (Sum Int)

one :: a -> Bag a 
one a = Map.singleton a (Sum 1)

addAll :: (Ord a , Foldable f) => f a -> Bag a -> Bag a
addAll = (<>) . fromFoldable

fromFoldable :: (Ord a , Foldable f) => f a -> Bag a 
fromFoldable = foldMap one

size :: Bag a -> Int
size = getSum . fold
