{-# LANGUAGE NoImplicitPrelude #-}

module Lib (recursing) where

import BasicPrelude

recursing :: Monad f => (a -> f ()) -> (a -> f b) -> a -> f b
recursing f recurse = (returning f) >=> recurse

returning :: Applicative f => (a -> f ()) -> a -> f a
returning f a = a <$ f a 
