{-# LANGUAGE TupleSections #-}

module Chess.Lib (guarded, singletonMaybe, withInput, Counts, one, getCount, traversefst) where

import Control.Applicative (Alternative)
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Sum (..))

returning :: (Functor f) => (t -> f b) -> t -> f t
returning f a = a <$ f a

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded f = returning (guard . f)

singletonMaybe :: [a] -> Maybe a
singletonMaybe [a] = Just a
singletonMaybe _ = Nothing

withInput :: (a -> b) -> a -> (a, b)
withInput f a = (a, f a)

type Counts a = Map a (Sum Int)

one :: (Num a) => k -> Map k (Sum a)
one a = Map.singleton a (Sum 1)

getCount :: (Num b, Ord k) => Map k (Sum b) -> k -> b
getCount m a = maybe 0 getSum $ Map.lookup a m

traversefst :: (Functor f) => (a -> f b) -> (a, c) -> f (b, c)
traversefst f (a, b) = (,b) <$> f a
