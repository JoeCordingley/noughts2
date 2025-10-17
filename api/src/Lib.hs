{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
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

composeMapWithInput :: (Ord a) => Map a b -> Map k a -> Map k (a, b)
composeMapWithInput = Map.mapMaybe . withInput . flip Map.lookup
  where
    withInput f a = (a,) <$> f a

fixAtomically :: ((a -> STM (IO b)) -> a -> STM (IO b)) -> a -> IO b
fixAtomically f = fix $ \recurse -> join . atomically . (f $ pure . recurse)

data PositionChoice a = PositionChoice {position :: a} | StartGame deriving (Generic, Show)

instance (FromJSON a) => FromJSON (PositionChoice a)

instance (ToJSON a) => ToJSON (PositionChoice a)

type ChooseRoleHtml a = Map a (PlayerId, Name) -> PlayerId -> Text
