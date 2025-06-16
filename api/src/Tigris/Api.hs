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
import Lib (encodeToText, keepAlive, recursing, returning, sendHtml)
import Lucid (term, toHtml)
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

data Role = Host | Guest deriving (Eq, Ord, Show)

data Player = Player Role Text deriving (Eq, Ord, Show)

data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)

instance FromJSON Dynasty

instance ToJSON Dynasty

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
  putStrLn "Running on http://localhost:8080/"
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
  generatedId <- generateGameId
  game <- newGame
  atomically $ modifyTVar games $ Map.insert generatedId game
  forkIO $ hostGame game
  pure generatedId

newGame :: IO Game
newGame = atomically $ do
  notify <- newTVar []
  ps <- newTVar []
  latestState <- newTVar Bimap.empty
  return Game {latestState = latestState, playerOutputs = notify, playerInputs = ps, waitForFinish = forever $ threadDelay 10000}

-- Game Joining

joinGame :: TVar GameMap -> GameId -> Maybe Text -> Server WebSocket
joinGame games gameId maybeCookies conn = do
  liftIO $ putStrLn "connected"
  gameList <- liftIO $ readTVarIO games
  case Map.lookup gameId gameList of
    Just game -> liftIO $ do
      player <- case maybeCookies >>= gameCreatorCookie of
        Just creatorId | creatorId == gameId -> newHostPlayer
        _ -> newGuestPlayer
      keepAlive conn $ addPlayer conn game player
    Nothing -> throwError err404

-- Add player and setup notification system
addPlayer :: Connection -> Game -> Player -> IO ()
addPlayer conn game player = do
  -- Create a personal notification queue
  outputQueue <- newTQueueIO
  inputQueue <- newTQueueIO
  -- Atomically register the player and queue
  state <- atomically $ do
    modifyTVar' (playerOutputs game) (writeTQueue outputQueue :)
    modifyTVar' (playerInputs game) ((player, readTQueue inputQueue) :)
    state <- readTVar (latestState game)
    writeTQueue outputQueue state
    return state
  putStrLn $ "state: " <> tshow state

  -- Start sender and receiver threads
  withAsync (sendLoop outputQueue conn player) $ \sender ->
    withAsync (readLoop conn inputQueue player) $ \reader ->
      waitForFinish game *> cancel sender *> cancel reader

-- Send updates from the queue to the connection
sendLoop :: TQueue PlayerMap -> Connection -> Player -> IO ()
sendLoop queue conn player = forever $ do
  state <- atomically $ readTQueue queue
  sendHtml conn $ chooseDynasty state player

chooseDynasty :: PlayerMap -> Player -> Html ()
chooseDynasty playerMap player =
  div_ [id_ "board"] $ do
    h2_ "Choose Your Dynasty"
    div_ [class_ "dynasty-buttons"]
      $ forM_ [Archer, Bull, Pot, Lion]
      $ \dynasty ->
        let isTaken = Bimap.member dynasty playerMap
            isMine = Bimap.lookup dynasty playerMap == Just player
            color
              | isMine = "green"
              | isTaken = "gray"
              | otherwise = "blue"
            jsonVal = encodeToText $ DynastyChoice dynasty
            attrs =
              [ style_ $ "background-color: " <> color <> "; margin: 0.5em; padding: 1em",
                term "hx-vals" jsonVal,
                term "ws-send" mempty
              ]
                ++ if isTaken && not isMine then [disabled_ "true"] else []
         in button_ attrs (toHtml $ show dynasty)

-- Dummy read loop (simulate receiving inputs)
readLoop :: Connection -> TQueue Dynasty -> Player -> IO ()
readLoop conn queue player = forever $ do
  msg <- receiveData conn
  case decode msg of
    Just (DynastyChoice dynasty) -> atomically $ writeTQueue queue $ dynasty
    Nothing -> do
      return () -- Handle decoding failure

-- Game Logic Core

hostGame :: Game -> IO ()
hostGame game = do
  playerMap <- fixAtomically notifyPlayers (setupGame receiveMsg) startingState
  playGame playerMap
  where
    receiveMsg = (readTVar $ playerInputs game) >>= firstPlayerMessage
    firstPlayerMessage = foldr orElse retry . map (uncurry $ fmap . TakePosition)
    notifyPlayers playerMap = notifySetup (readTVar $ playerOutputs game) playerMap *> writeTVar (latestState game) playerMap
    startingState = Bimap.empty

notifySetup :: STM [PlayerMap -> STM ()] -> PlayerMap -> STM ()
notifySetup playersVar playerMap = playersVar >>= traverse_ ($ playerMap)

fixAtomically :: (a -> STM b) -> ((a -> STM a) -> a -> STM a) -> a -> IO a
fixAtomically notify f = atomically . (returning notify) >=> loop
  where
    loop = atomically . f (returning notify) >=> loop

setupGame :: (Monad m) => m SetupMessage -> (PlayerMap -> m PlayerMap) -> PlayerMap -> m PlayerMap
setupGame receive recurse playerMap = do
  message <- receive
  case message of
    TakePosition player position ->
      recurse $ takePosition playerMap
      where
        takePosition = if Bimap.notMember position playerMap then Bimap.insert position player else id

playGame :: PlayerMap -> IO ()
playGame = undefined

-- WebSocket Helpers

gameCreator :: Text
gameCreator = "gameCreator"

cookie :: Text -> Text -> Text
cookie key value = key <> "=" <> value

gameCreatorCookie :: Text -> Maybe GameId
gameCreatorCookie text = do
  creatorId <- lookup gameCreator $ parseCookiesText $ TE.encodeUtf8 text
  return $ GameId creatorId

-- ID and Player Generation

generateId :: IO Text
generateId = stringRandomIO "[a-zA-Z0-9]{5}"

generateGameId :: IO GameId
generateGameId = GameId <$> generateId

newGuestPlayer :: IO Player
newGuestPlayer = Player Guest <$> generateId

newHostPlayer :: IO Player
newHostPlayer = Player Host <$> generateId

data DynastyChoice = DynastyChoice {dynasty :: Dynasty} deriving (Generic)

instance ToJSON DynastyChoice

instance FromJSON DynastyChoice
