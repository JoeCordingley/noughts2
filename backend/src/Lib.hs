module Lib where

import Control.Applicative (Alternative)
import Control.Monad (guard)

returning :: (Functor f) => (t -> f b) -> t -> f t
returning f a = a <$ f a

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded f = returning (guard . f)
