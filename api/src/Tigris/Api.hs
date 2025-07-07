{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tigris.Api (runServer) where

import BasicPrelude
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.STM
import Control.Monad.Error.Class (MonadError)
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

type CreateGameResponse = Headers '[Header "HX-Redirect" Text, Header "Set-Cookie" Text, Header "Set-Cookie" Text] NoContent

type GameResponse = Html ()

type JoinGameResponse = Headers '[Header "Set-Cookie" Text] (Html ())

type API =
  "api"
    :> "tigris"
    :> ( "create" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[JSON] CreateGameResponse
           :<|> Capture "gameId" GameId
             :> ( Header "Cookie" Text :> Get '[HTML] GameResponse
                    :<|> "join" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[HTML] JoinGameResponse
                    :<|> "play" :> Header "Cookie" Text :> WebSocket
                )
       )

createGame :: TVar GameMap -> [(Text, Text)] -> Handler CreateGameResponse
createGame games formData = case lookup "name" formData of
  Just name -> do
    liftIO $ putStrLn $ "Creating game for player: " <> name
    GameId newId <- liftIO $ createNewGame games
    return $ addHeader ("/games/tigris/" <> newId) $ addHeader (cookie "name" name) $ addHeader (cookie gameCreator newId) NoContent
  Nothing -> throwError err400

cookie :: Text -> Text -> Text
cookie key value = key <> "=" <> value

playGameHandler :: TVar GameMap -> GameId -> Maybe Text -> Server WebSocket
playGameHandler games gameId maybeCookies conn = withGame games gameId $ liftIO . connectGame gameId maybeCookies conn

connectGame :: GameId -> Maybe Text -> Connection -> Game -> IO ()
connectGame gameId maybeCookies conn game = do
  putStrLn "connected"
  player <- case maybeCookies >>= gameCreatorCookie of
    Just creatorId | creatorId == gameId -> newHostPlayer
    _ -> newGuestPlayer
  keepAlive conn $ addPlayer conn game player

-- Server Setup

runServer :: IO ()
runServer = do
  games <- newTVarIO Map.empty
  putStrLn "Running on http://localhost:8080/"
  run 8080 (serve (Proxy :: Proxy API) (server games))

server :: TVar GameMap -> Server API
server games = createGame games :<|> gameEndpoints
  where
    gameEndpoints id = gameHandler games id :<|> joinGame games id :<|> playGameHandler games id

gameHandler :: TVar GameMap -> GameId -> Maybe Text -> Handler GameResponse
gameHandler games id maybeCookies = withGame games id f
  where
    f game = return $ case maybeCookies >>= nameCookie of
      Just name -> websocketHtml id
      Nothing ->
        form_ [term "hx-post" ("/api/tigris/" <> gameId id <> "/join")] $ do
          input_ [id_ "name", name_ "name", type_ "text"]
          button_ [type_ "submit"] "Join"

websocketHtml :: GameId -> Html ()
websocketHtml id = div_ [id_ "game", term "hx-ext" "ws", term "ws-connect" ("/api/tigris/" <> gameId id <> "/play")] $ div_ [id_ "board"] $ return ()

joinGame :: TVar GameMap -> GameId -> [(Text, Text)] -> Handler JoinGameResponse
joinGame games id formData = withGame games id $ const $ case lookup "name" formData of
  Just name -> return $ addHeader (cookie "name" name) (websocketHtml id)
  Nothing -> throwError err400

withGame ::
  (MonadIO f, MonadError ServerError f) =>
  TVar GameMap ->
  GameId ->
  (Game -> f a) ->
  f a
withGame gamesVar gameId action = do
  gameMap <- liftIO $ readTVarIO gamesVar
  case Map.lookup gameId gameMap of
    Just game -> action game
    Nothing -> throwError err404

-- Game Creation

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

gameCreatorCookie :: Text -> Maybe GameId
gameCreatorCookie = fmap GameId . getCookie gameCreator

getCookie :: Text -> Text -> Maybe Text
getCookie key =
  lookup key . parseCookiesText . TE.encodeUtf8

nameCookie :: Text -> Maybe ()
nameCookie = void . getCookie "name"

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
