{-# LANGUAGE NoImplicitPrelude #-}

module Lib (recursing, keepAlive, returning, sendHtml) where

import BasicPrelude
import Lucid.Base (Html, renderText)
import Network.WebSockets.Connection (Connection, sendTextData, withPingThread)

recursing :: (Monad f) => (a -> f ()) -> (a -> f b) -> a -> f b
recursing f recurse = (returning f) >=> recurse

returning :: (Applicative f) => (a -> f ()) -> a -> f a
returning f a = a <$ f a

keepAlive :: Connection -> IO c -> IO c
keepAlive conn =
  withPingThread conn 30 (pure ())

sendHtml :: Connection -> Html () -> IO ()
sendHtml conn html = sendTextData conn $ renderText html
