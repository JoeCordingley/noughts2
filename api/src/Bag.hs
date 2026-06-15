{-# LANGUAGE NoImplicitPrelude #-}

module Bag (Bag, one, addAll, fromFoldable, size, insert, lookup) where

import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Sum (..))
import Prelude hiding (lookup)

type Bag a = Map a (Sum Int)

one :: a -> Bag a
one a = Map.singleton a (Sum 1)

addAll :: (Ord a, Foldable f) => f a -> Bag a -> Bag a
addAll = (<>) . fromFoldable

insert :: (Ord a) => a -> Bag a -> Bag a
insert a = (<> one a)

fromFoldable :: (Ord a, Foldable f) => f a -> Bag a
fromFoldable = foldMap one

size :: Bag a -> Sum Int
size = fold

lookup :: (Ord a) => a -> Bag a -> Sum Int
lookup = Map.findWithDefault 0
