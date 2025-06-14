{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tigris.Api (runServer) where

import BasicPrelude
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.STM
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.ByteString.Lazy as BL
import Data.Function (fix)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import Lib (recursing, returning)
import Lucid.Base (Html)
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Network.WebSockets.Connection (Connection, receiveData, sendTextData)
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.WebSocket (WebSocket)
import Text.StringRandom (stringRandomIO)
import Web.Cookie (parseCookiesText)

-- Types

type CreateGameResponse = Headers '[Header "HX-Redirect" Text, Header "Set-Cookie" Text] NoContent

data SetupMessage = TakePosition Player Dynasty deriving (Generic, Show)

newtype Player = Player Text deriving (Eq, Ord, Show)

data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)

instance FromJSON Dynasty

data GameId = GameId {gameId :: Text} deriving (Eq, Show, Ord)

instance FromHttpApiData GameId where
  parseUrlPiece = Right . GameId

type PlayerMap = Bimap Dynasty Player

type NotifyPlayer = PlayerMap -> IO ()

data Game = Game
  { latestState :: TVar PlayerMap,
    playerOutputs :: TVar [PlayerMap -> STM ()],
    playerInputs :: TVar [(Player, STM Dynasty)],
    waitForFinish :: IO ()
  }

type GameMap = Map.Map GameId Game

-- Servant API

type API =
  "api"
    :> "tigris"
    :> ( "create" :> Post '[JSON] CreateGameResponse
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
  return Game {playerOutputs = notify, playerInputs = ps, waitForFinish = forever $ threadDelay 10000}

-- Game Joining

joinGame :: TVar GameMap -> GameId -> Maybe Text -> Server WebSocket
joinGame games gameId maybeCookies conn = do
  gameList <- liftIO $ readTVarIO games
  case Map.lookup gameId gameList of
    Just game -> liftIO $ newPlayer >>= addPlayer conn game
    Nothing -> throwError err404

-- Add player and setup notification system
addPlayer :: Connection -> Game -> Player -> IO ()
addPlayer conn game player = do
  -- Create a personal notification queue
  outputQueue <- newTQueueIO
  inputQueue <- newTQueueIO
  -- Atomically register the player and queue
  atomically $ do
    modifyTVar' (playerOutputs game) (writeTQueue outputQueue :)
    modifyTVar' (playerInputs game) ((player, readTQueue inputQueue) :)
    state <- readTVar (latestState game)
    writeTQueue outputQueue state

  -- Start sender and receiver threads
  withAsync (sendLoop outputQueue conn) $ \sender ->
    withAsync (readLoop conn inputQueue player) $ \reader ->
      waitForFinish game *> cancel sender *> cancel reader

-- Send updates from the queue to the connection
sendLoop :: TQueue PlayerMap -> Connection -> IO ()
sendLoop queue conn = forever $ do
  state <- atomically $ readTQueue queue
  sendTextData conn (tshow state)

-- Dummy read loop (simulate receiving inputs)
readLoop :: Connection -> TQueue Dynasty -> Player -> IO ()
readLoop conn queue player = forever $ do
  msg <- receiveData conn
  case decode msg of
    Just dynasty -> atomically $ writeTQueue queue $ dynasty
    Nothing -> return () -- Handle decoding failure

-- decodeSetupMessage :: Player -> BL.ByteString -> Maybe SetupMessage
-- decodeSetupMessage player =

-- Normally you decode and write to a shared queue for the game

-- Update all clients
-- broadcastUpdate :: Game -> PlayerMap -> IO ()
-- broadcastUpdate game newState = atomically $ do
--   writeTVar (latestState game) newState
--   notifiers <- readTVar (playerOutputs game)
--   mapM_ (`writeTQueue` newState) notifiers

-- joinGame :: TVar GameMap -> GameId -> Maybe Text -> Server WebSocket
-- joinGame games gameId maybeCookies conn = do
--  gameList <- liftIO $ readTVarIO games
--  case Map.lookup gameId gameList of
--    Just game -> liftIO $ addPlayer conn game
--    Nothing -> throwError err404
--  where
--    addPlayer conn game = do
--      player <- newPlayer
--      queue <- atomically $ do
--        q <- newTQueue
--        modifyTVar' (players game) (readTQueue q :)
--        return q
--      withAsync (readFromWebSocket (decodeSetupMessage player) conn queue) $ \reader ->
--        waitForFinish game *> cancel reader

-- Game Logic Core

hostGame :: Game -> IO ()
hostGame game = do
  playerMap <- setupGame receiveMsg notifyPlayers Bimap.empty
  playGame playerMap
  where
    receiveMsg = (readTVar $ playerInputs game) >>= firstPlayerMessage
    notifyPlayers = notifySetup (readTVar $ playerOutputs game)

firstPlayerMessage :: [(Player, STM Dynasty)] -> STM SetupMessage
firstPlayerMessage = foldr orElse retry . map (uncurry $ fmap . TakePosition)

notifySetup :: STM [PlayerMap -> STM ()] -> PlayerMap -> STM ()
notifySetup playersVar playerMap = playersVar >>= traverse_ ($ playerMap)

setupGame :: STM SetupMessage -> (PlayerMap -> STM ()) -> PlayerMap -> IO PlayerMap
setupGame receive notify playerMap = do
  receive' >>= setupGame'
  where
    receive' = atomically $ do
      message <- receive
      case message of
        TakePosition player position ->
          returning notify $ takePosition playerMap
          where
            takePosition = if Bimap.notMember position playerMap then Bimap.insert position player else id
    setupGame' = setupGame receive notify

playGame :: PlayerMap -> IO ()
playGame = undefined

-- WebSocket Helpers

readFromWebSocket :: (BL.ByteString -> Maybe a) -> Connection -> TQueue a -> IO ()
readFromWebSocket decoder conn queue = forever $ do
  msg <- receiveData conn
  traverse_ (atomically . writeTQueue queue) (decoder msg)

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
