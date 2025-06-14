{-# LANGUAGE NoImplicitPrelude #-}

module Lib (recursing, keepAlive, returning) where

import BasicPrelude
import Network.WebSockets.Connection (Connection, withPingThread)

recursing :: (Monad f) => (a -> f ()) -> (a -> f b) -> a -> f b
recursing f recurse = (returning f) >=> recurse

returning :: (Applicative f) => (a -> f ()) -> a -> f a
returning f a = a <$ f a

keepAlive :: Connection -> IO c -> IO c
keepAlive conn =
  withPingThread conn 30 (pure ())
