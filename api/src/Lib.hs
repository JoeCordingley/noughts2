{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import BasicPrelude
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.STM (STM, TQueue, TVar, atomically, modifyTVar, modifyTVar', newTQueueIO, newTVar, readTQueue, readTVar, readTVarIO, writeTQueue, writeTVar)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Function (fix)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import Lucid.Base (Html, renderText)
import Network.WebSockets.Connection (Connection, receiveData, sendTextData, withPingThread)
import Servant
import Servant.API.WebSocket (WebSocket)
import Text.StringRandom (stringRandomIO)

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
  toUrlPiece (GameId id) = id

composeMapWithInput :: (Ord a) => Map a b -> Map k a -> Map k (a, b)
composeMapWithInput = Map.mapMaybe . withInput . flip Map.lookup
  where
    withInput f a = (a,) <$> f a

fixAtomically :: ((a -> STM (IO b)) -> a -> STM (IO b)) -> a -> IO b
fixAtomically f = fix $ \recurse -> join . atomically . (f $ pure . recurse)

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

-- data GameServerDependencies input state = GameServerDependencies
--   { gameKey :: GameKey,
--     playGame :: state -> IO (),
--     setup :: STM input -> (state -> STM ()) -> state -> IO state,
--     openingState :: state,
--     showState :: Player -> state -> Html ()
--   }

data GameServerDependencies = GameServerDependencies
  { gameKey :: GameKey,
    gameActions :: IO Actions
  }

data Actions = Actions {createGame :: Player -> IO GameId, newPlayerId :: IO PlayerId, getGame :: GameId -> IO (Maybe Table)}

data Table = Table {addPlayer :: Player -> IO (), connectGame :: Player -> Connection -> IO (), tablePlayers :: PlayerId -> IO (Maybe Player)}

data Player = Player {playerId :: PlayerId, playerName :: Name} deriving (Show, Eq)

type Unfixed a b = (a -> b) -> a -> b

type UnfixedSetup s = Unfixed s (STM (IO s))

instance HasLink WebSocket where
  type MkLink WebSocket a = a
  toLink f _ segments = f segments

data GameTVars state input = GameTVars
  { latestState :: TVar state,
    playerOutputs :: TVar [state -> STM ()],
    playerInputs :: TVar [(Player, STM input)],
    playerNames :: TVar (Map PlayerId Player),
    waitForFinish :: IO ()
  }

seatPlayers :: GameTVars state input -> ((state -> STM (IO state)) -> state -> STM (IO state)) -> state -> IO state
seatPlayers game setupGame startingState = updateWithNotify setupGame notify startingState
  where
    notify state = do
      readTVar (playerOutputs game) >>= traverse_ ($ state)
      writeTVar (latestState game) $ state

updateWithNotify :: ((a -> STM (IO b)) -> a -> STM (IO b)) -> (a -> STM ()) -> a -> IO b
updateWithNotify update notify = (fixAtomically $ update . notifying) <=< returning (atomically . notify)
  where
    notifying recurse = returning notify >=> recurse

actions :: (Player -> STM game) -> (game -> IO ()) -> (game -> Table) -> TVar (Map GameId game) -> Actions
actions newGame hostGame table gameMap = Actions {createGame, newPlayerId, getGame}
  where
    newPlayerId = PlayerId <$> generateId
    createGame player = do
      gameId <- generateGameId
      game <- atomically $ returning (modifyTVar gameMap . Map.insert gameId) =<< newGame player
      forkIO $ hostGame game
      pure gameId
    getGame gameId = (fmap table . Map.lookup gameId) <$> readTVarIO gameMap

newGame :: state -> Player -> STM (GameTVars state input)
newGame openingState player = do
  notify <- newTVar []
  ps <- newTVar []
  latestState <- newTVar openingState
  names <- newTVar $ Map.singleton (playerId player) player
  return GameTVars {latestState = latestState, playerOutputs = notify, playerInputs = ps, waitForFinish = forever $ threadDelay 10000, playerNames = names}

table :: (Show input, FromJSON input, Show state) => (Player -> state -> Html ()) -> GameTVars state input -> Table
table playerView tvars = Table {addPlayer, connectGame, tablePlayers}
  where
    addPlayer player = atomically $ addPlayerSTM player tvars
    tablePlayers id = atomically . fmap (Map.lookup id) . readTVar $ playerNames tvars
    connectGame player connection = do
      putStrLn "connected"
      outputQueue <- newTQueueIO
      inputQueue <- newTQueueIO
      atomically $ do
        modifyTVar' (playerOutputs tvars) (writeTQueue outputQueue :)
        modifyTVar' (playerInputs tvars) ((player, readTQueue inputQueue) :)
        state <- readTVar (latestState tvars)
        writeTQueue outputQueue state
      withAsync (sendLoop playerView outputQueue connection player) $ \sender ->
        withAsync (readLoop connection inputQueue) $ \reader ->
          waitForFinish tvars *> cancel sender *> cancel reader

readLoop :: (FromJSON a, Show a) => Connection -> TQueue a -> IO ()
readLoop conn queue = forever $ do
  msg <- receiveData conn
  putStrLn $ "msg: " <> tshow msg
  case decode msg of
    Just message -> do
      putStrLn $ "message: " <> tshow message
      atomically $ writeTQueue queue $ message
    Nothing -> do
      return () -- Handle decoding failure

sendLoop :: (Show state) => (Player -> state -> Html ()) -> TQueue state -> Connection -> Player -> IO ()
sendLoop playerView queue conn player = forever $ do
  state <- atomically $ readTQueue queue
  putStrLn $ "sending" <> tshow state
  sendHtml conn $ playerView player state

addPlayerSTM :: Player -> GameTVars state input -> STM ()
addPlayerSTM player = flip modifyTVar' (Map.insert (playerId player) player) . playerNames
