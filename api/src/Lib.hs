{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import BasicPrelude
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Lucid.Base (Html, renderText)
import Network.WebSockets.Connection (Connection, sendTextData, withPingThread)

recursing :: (Monad f) => (a -> f ()) -> (a -> f b) -> a -> f b
recursing f recurse = (returning f) >=> recurse

returning :: (Applicative f) => (a -> f b) -> a -> f a
returning f a = a <$ f a

keepAlive :: Connection -> IO c -> IO c
keepAlive conn =
  withPingThread conn 30 (pure ())

sendHtml :: Connection -> Html () -> IO ()
sendHtml conn html = sendTextData conn $ renderText html

encodeToText :: (ToJSON a) => a -> Text
encodeToText = TL.toStrict . encodeToLazyText

data PlayerId = PlayerId Text deriving (Eq, Ord, Show)

type Name = Text

instance (ToJSON a) => ToJSON (CharacterChoice a)

instance (FromJSON a) => FromJSON (CharacterChoice a)

data CharacterChoice a = CharacterChoice {character :: a} deriving (Generic)

data GameId = GameId {gameId :: Text} deriving (Eq, Show, Ord)
