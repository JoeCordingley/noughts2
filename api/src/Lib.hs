{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import BasicPrelude
import Control.Concurrent.STM (STM, atomically)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Function (fix)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Lucid.Base (Html, renderText)
import Network.WebSockets.Connection (Connection, sendTextData, withPingThread)
import Servant
import Servant.API.WebSocket (WebSocket)

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

data GameId = GameId {gameId :: Text} deriving (Eq, Show, Ord)

instance FromHttpApiData GameId where
  parseUrlPiece = Right . GameId

instance ToHttpApiData GameId where
  toUrlPiece (GameId id) = id

composeMapWithInput :: (Ord a) => Map a b -> Map k a -> Map k (a, b)
composeMapWithInput = Map.mapMaybe . withInput . flip Map.lookup
  where
    withInput f a = (a,) <$> f a

fixAtomically :: ((a -> STM (IO b)) -> a -> STM (IO b)) -> a -> IO b
fixAtomically f = fix $ \recurse -> join . atomically . (f $ pure . recurse)

data PositionChoice a = PositionChoice {position :: a} | StartGame deriving (Generic, Show)

instance (FromJSON a) => FromJSON (PositionChoice a)

instance (ToJSON a) => ToJSON (PositionChoice a)

type ChooseRoleHtml a = Map a (PlayerId, Name) -> PlayerId -> (Html ())

instance FromJSON NoughtOrCross

data NoughtOrCross = Nought | Cross deriving (Eq, Ord, Show, Generic)

data GameKey = Tigris | Noughts

instance FromHttpApiData GameKey where
  parseUrlPiece value = case value of
    "tigris" -> Right Tigris
    "noughts" -> Right Noughts
    other -> Left $ "invalid game" <> other

instance ToHttpApiData GameKey where
  toUrlPiece Tigris = "tigris"
  toUrlPiece Noughts = "noughts"

data GameServerDependencies a = GameServerDependencies
  { gameKey :: GameKey,
    playGame :: Map a PlayerId -> IO (),
    isReady :: Map a PlayerId -> Bool,
    chooseRole :: Map a (PlayerId, Name) -> PlayerId -> (Html ())
  }

instance HasLink WebSocket where
  type MkLink WebSocket a = a
  toLink f _ segments = f segments
