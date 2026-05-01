{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib (ChooseRoleHtml, NoughtOrCross, PlayerId(..), keepAlive, sendHtml, encodeToText, recursing, GameId, Actions(..), Player(..), returning, Table(..), GameTVars(..), GameKey(..), actions, newGame, table, nextMessageFromAnyPlayer, notify, WebSocketContainer(..), gameDiv) where

import BasicPrelude
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, modifyTVar', newTQueueIO, newTVar, orElse, readTQueue, readTVar, readTVarIO, retry, writeTQueue, writeTVar)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import Lucid.Base (Html, renderText)
import Network.WebSockets.Connection (Connection, receiveData, sendTextData, withPingThread)
import Servant
import Servant.API.WebSocket (WebSocket)
import Text.StringRandom (stringRandomIO)
import Lucid.Html5


generateGameId :: IO GameId
generateGameId = GameId <$> generateId

generateId :: IO Text
generateId = stringRandomIO "[a-zA-Z0-9]{5}"

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
  toUrlPiece (GameId id') = id'


type ChooseRoleHtml a = Map a (PlayerId, Name) -> PlayerId -> (Html ())

instance FromJSON NoughtOrCross

data NoughtOrCross = Nought | Cross deriving (Eq, Ord, Show, Generic)

data GameKey = Tigris | Noughts | Chess

instance FromHttpApiData GameKey where
  parseUrlPiece value = case value of
    "tigris" -> Right Tigris
    "noughts" -> Right Noughts
    "chess" -> Right Chess
    other -> Left $ "invalid game " <> other

instance ToHttpApiData GameKey where
  toUrlPiece Tigris = "tigris"
  toUrlPiece Noughts = "noughts"
  toUrlPiece Chess = "chess"


data Actions = Actions {createGame :: Player -> IO GameId, newPlayerId :: IO PlayerId, getGame :: GameId -> IO (Maybe Table)}

data Table = Table {addPlayer :: Player -> IO (), connectGame :: Player -> Connection -> IO (), tablePlayer :: PlayerId -> IO (Maybe Player)}

data Player = Player {playerId :: PlayerId, playerName :: Name} deriving (Show, Eq)

instance HasLink WebSocket where
  type MkLink WebSocket a = a
  toLink f _ segments = f segments

instance HasLink WebSocketContainer where
  type MkLink WebSocketContainer a = a
  toLink f _ segments = f segments

newtype WebSocketContainer = WebSocketContainer {getWebsocket :: WebSocket}

data GameTVars state input = GameTVars
  { latestState :: TVar state,
    playerOutputs :: TVar [state -> STM ()],
    playerInputs :: TVar [(Player, STM input)],
    playerNames :: TVar (Map PlayerId Player),
    waitForFinish :: IO ()
  }

notify :: GameTVars state input -> state -> STM ()
notify game state = do
  readTVar (playerOutputs game) >>= traverse_ ($ state)
  writeTVar (latestState game) $ state


actions :: (Player -> STM game) -> (game -> IO ()) -> (game -> Table) -> TVar (Map GameId game) -> Actions
actions newGame' hostGame table' gameMap = Actions {createGame, newPlayerId, getGame}
  where
    newPlayerId = PlayerId <$> generateId
    createGame player = do
      gameId <- generateGameId
      game <- atomically $ returning (modifyTVar gameMap . Map.insert gameId) =<< newGame' player
      _ <- forkIO $ hostGame game
      pure gameId
    getGame gameId = (fmap table' . Map.lookup gameId) <$> readTVarIO gameMap

newGame :: state -> Player -> STM (GameTVars state input)
newGame openingState player = do
  notify' <- newTVar []
  ps <- newTVar []
  latestState <- newTVar openingState
  names <- newTVar $ Map.singleton (playerId player) player
  return GameTVars {latestState = latestState, playerOutputs = notify', playerInputs = ps, waitForFinish = forever $ threadDelay 10000, playerNames = names}

table :: FromJSON input => (Player -> state -> Html ()) -> GameTVars state input -> Table
table playerView tvars = Table {addPlayer, connectGame, tablePlayer}
  where
    addPlayer player = atomically $ addPlayerSTM player
    addPlayerSTM player = flip modifyTVar' (Map.insert (playerId player) player) $ playerNames tvars
    tablePlayer id' = atomically . fmap (Map.lookup id') . readTVar $ playerNames tvars
    connectGame player connection = do
      putStrLn "connected"
      outputQueue <- newTQueueIO
      inputQueue <- newTQueueIO
      atomically $ do
        modifyTVar' (playerOutputs tvars) (writeTQueue outputQueue :)
        modifyTVar' (playerInputs tvars) ((player, readTQueue inputQueue) :)
        state <- readTVar (latestState tvars)
        writeTQueue outputQueue state
      withAsync (sendLoop (fmap (playerView player) $ readTQueue outputQueue) connection) $ \sender ->
        withAsync (readLoop connection (writeTQueue inputQueue)) $ \reader ->
          waitForFinish tvars *> cancel sender *> cancel reader

readLoop :: (FromJSON a) => Connection -> (a -> STM ()) -> IO ()
readLoop conn enqueue = forever $ do
  msg <- receiveData conn
  putStrLn $ tshow msg
  case decode msg of
    Just message -> do
      atomically . enqueue $ message
    Nothing -> do
      return () -- Handle decoding failure

sendLoop :: STM (Html ()) -> Connection -> IO ()
sendLoop dequeue conn = forever $ sendHtml conn =<< atomically dequeue

nextMessageFromAnyPlayer :: TVar [(Player, STM input)] -> STM (Player, input)
nextMessageFromAnyPlayer playerInputs = readTVar playerInputs >>= nextPlayerMessage
  where
    nextPlayerMessage = foldr orElse retry . map sequence

gameDiv :: Html () -> Html ()
gameDiv = div_ [id_ "game"] 
