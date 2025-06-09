{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Tigris.Api (runServer) where

import BasicPrelude
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.Async (withAsync, cancel)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import Data.Function (fix)
import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Lucid.Html5
import Lucid.Base (Html)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.WebSocket (WebSocket)
import Network.WebSockets.Connection (Connection, receiveData, sendTextData)
import Text.StringRandom (stringRandomIO)
import Web.Cookie (parseCookiesText)
import GHC.Generics (Generic)
import Lib (recursing)
import GHC.Conc (threadDelay)

-- Types

type CreateGameResponse = Headers '[Header "HX-Redirect" Text, Header "Set-Cookie" Text] NoContent

data SetupMessage = TakePosition Player Dynasty deriving (Generic)

newtype Player = Player Text deriving (Eq, Ord)

data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic)
instance FromJSON Dynasty

data GameId = GameId { gameId :: Text } deriving (Eq, Show, Ord)
instance FromHttpApiData GameId where
  parseUrlPiece = Right . GameId

type PlayerMap = Bimap Dynasty Player

type NotifyPlayer = PlayerMap -> IO ()

data Game = Game
  { notifyPlayers :: TVar [NotifyPlayer]
  , waitForFinish :: IO ()
  , players :: TVar [STM SetupMessage]
  }

type GameMap = Map.Map GameId Game

-- Servant API

type API =
  "api" :> "tigris" :>
    (    "create" :> Post '[JSON] CreateGameResponse
    :<|> Capture "gameId" GameId :> Header "Cookie" Text :> WebSocket
    )

-- Server Setup

runServer :: IO ()
runServer = do
  games <- newTVarIO Map.empty
  run 8080 (serve (Proxy :: Proxy API) (server games))

server :: TVar GameMap -> Server API
server games = createGame games :<|> joinGame games

-- Game Creation

createGame :: TVar GameMap -> Handler CreateGameResponse
createGame games = do
  GameId newId <- liftIO $ createNewGame games
  return $ addHeader ("/games/tigris/" <> newId) $ addHeader (cookie gameCreator newId) NoContent

createNewGame :: TVar GameMap -> IO GameId
createNewGame games = do
  generatedId <- generateId
  game <- newGame
  atomically $ modifyTVar games $ Map.insert generatedId game
  forkIO $ hostGame game
  pure generatedId

newGame :: IO Game
newGame = atomically $ do
  notify <- newTVar []
  ps <- newTVar []
  return Game { notifyPlayers = notify, players = ps, waitForFinish = forever $ threadDelay 10000 }

-- Game Joining

joinGame :: TVar GameMap -> GameId -> Maybe Text -> Server WebSocket
joinGame games gameId maybeCookies conn = do
  gameList <- liftIO $ readTVarIO games
  case Map.lookup gameId gameList of
    Just game -> liftIO $ addPlayer conn game
    Nothing -> throwError err404
  where
    addPlayer conn game = do
      player <- newPlayer
      queue <- atomically $ do
        q <- newTQueue
        modifyTVar' (players game) (readTQueue q :)
        return q
      withAsync (readFromWebSocket (decodeSetupMessage player) conn queue) $ \reader ->
        waitForFinish game *> cancel reader

-- Game Logic Core

hostGame :: Game -> IO ()
hostGame game = do
  playerMap <- fix (recursing (notifySetup $ notifyPlayers game) . setupGame (receiveMsg $ players game)) Bimap.empty
  playGame playerMap

receiveMsg :: TVar [STM SetupMessage] -> IO SetupMessage
receiveMsg players = atomically $ do
  ps <- readTVar players
  foldr orElse retry ps

notifySetup :: TVar [NotifyPlayer] -> PlayerMap -> IO ()
notifySetup playersVar playerMap = do
  players <- atomically $ readTVar playersVar
  traverse_ ($ playerMap) players

setupGame :: (Monad m) => m SetupMessage -> (PlayerMap -> m PlayerMap) -> PlayerMap -> m PlayerMap
setupGame receive recurse playerMap = do
  message <- receive
  case message of
    TakePosition player position -> recurse $ takePosition player position playerMap
  where
    takePosition player position pm
      | Bimap.notMember position pm = Bimap.insert position player pm
      | otherwise = pm

playGame :: PlayerMap -> IO ()
playGame = undefined

-- WebSocket Helpers

readFromWebSocket :: (BL.ByteString -> Maybe a) -> Connection -> TQueue a -> IO ()
readFromWebSocket decoder conn queue = forever $ do
  msg <- receiveData conn
  traverse_ (atomically . writeTQueue queue) (decoder msg)

decodeSetupMessage :: Player -> BL.ByteString -> Maybe SetupMessage
decodeSetupMessage player = fmap (TakePosition player) . decode

-- Cookie Helpers

gameCreator :: Text
gameCreator = "gameCreator"

cookie :: Text -> Text -> Text
cookie key value = key <> "=" <> value

gameCreatorCookie :: Text -> Maybe GameId
gameCreatorCookie text = do
  creatorId <- lookup gameCreator $ parseCookiesText $ TE.encodeUtf8 text
  return $ GameId creatorId

-- ID and Player Generation

generateId :: IO GameId
generateId = GameId <$> stringRandomIO "[a-zA-Z0-9]{5}"

newPlayer :: IO Player
newPlayer = Player <$> stringRandomIO "[a-zA-Z0-9]{5}"
