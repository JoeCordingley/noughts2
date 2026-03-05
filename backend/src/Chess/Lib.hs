{-# LANGUAGE TupleSections #-}

module Chess.Lib (guarded, singletonMaybe, withInput, Counts, one, getCount, traversefst, foldrM, withInputF, takeWhileIncludingFirstFailure, firstJust, both) where

import Control.Applicative (Alternative)
import Control.Monad (guard, (>=>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (First (..), Sum (..))

returning :: (Functor f) => (t -> f b) -> t -> f t
returning f a = a <$ f a

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded f = returning (guard . f)

singletonMaybe :: [a] -> Maybe a
singletonMaybe [a] = Just a
singletonMaybe _ = Nothing

withInput :: (a -> b) -> a -> (a, b)
withInput f a = (a, f a)

withInputF :: (Functor f) => (t -> f a) -> t -> f (t, a)
withInputF f a = (a,) <$> f a

type Counts a = Map a (Sum Int)

one :: (Num a) => k -> Map k (Sum a)
one a = Map.singleton a (Sum 1)

getCount :: (Num b, Ord k) => Map k (Sum b) -> k -> b
getCount m a = maybe 0 getSum $ Map.lookup a m

traversefst :: (Functor f) => (a -> f b) -> (a, c) -> f (b, c)
traversefst f (a, b) = (,b) <$> f a

foldrM :: (Foldable t, Monad m) => (a -> c -> m c) -> t a -> c -> m c
foldrM f = foldr ((>=>) . f) pure

takeWhileIncludingFirstFailure :: (a -> Bool) -> [a] -> [a]
takeWhileIncludingFirstFailure _ [] = []
takeWhileIncludingFirstFailure p (a : as) = a : if p a then takeWhileIncludingFirstFailure p as else []

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = fmap getFirst . foldMap (fmap First . f)

both :: (t -> b) -> (t, t) -> (b, b)
both f (x, y) = (f x, f y)
