{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tigris.Api (runServer, Dynasty) where

import BasicPrelude
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.STM
import Control.Monad.Error.Class (MonadError)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Function (fix)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import Lib
import Lucid (term, toHtml)
import Lucid.Base (Html)
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Network.WebSockets.Connection (Connection, receiveData)
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.WebSocket (WebSocket)
import Text.StringRandom (stringRandomIO)
import Web.Cookie (parseCookiesText)

-- Types

data SetupMessage = TakePosition PlayerId Dynasty | StartGame deriving (Generic, Show)

data Role = Host | Guest deriving (Eq, Ord, Show)

data Player = Player Role PlayerId Name deriving (Eq, Ord, Show)

data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)

instance FromJSON Dynasty

instance ToJSON Dynasty

instance FromHttpApiData GameId where
  parseUrlPiece = Right . GameId

type PlayerMap = Map Dynasty PlayerId

type NotifyPlayer = PlayerMap -> IO ()

data Game = Game
  { latestState :: TVar (Map Dynasty (PlayerId, Name)),
    playerOutputs :: TVar [Map Dynasty (PlayerId, Name) -> STM ()],
    playerInputs :: TVar [(PlayerId, STM Dynasty)],
    playerNames :: TVar (Map PlayerId Name),
    waitForFinish :: IO ()
  }

type GameMap = Map.Map GameId Game

-- Servant API

type CreateGameResponse = Headers '[Header "HX-Redirect" Text, Header "Set-Cookie" Text] NoContent

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

addPlayerIdCookie :: (AddHeader [Optional, Strict] h Text orig new) => PlayerId -> orig -> new
addPlayerIdCookie (PlayerId playerId) = addHeader (cookie playerIdKey playerId)

createGame :: TVar GameMap -> [(Text, Text)] -> Handler CreateGameResponse
createGame games formData = case lookup "name" formData of
  Just name -> liftIO $ do
    putStrLn $ "Creating game for player: " <> name
    playerId <- newPlayerId
    GameId newId <- createNewGame games playerId name
    return $ addHeader ("/games/tigris/" <> newId) $ addPlayerIdCookie playerId NoContent
  Nothing -> throwError err400

cookie :: Text -> Text -> Text
cookie key value = key <> "=" <> value

playGameHandler :: (MonadIO f, MonadError ServerError f) => TVar GameMap -> GameId -> Maybe Text -> Connection -> f ()
playGameHandler games gameId maybeCookies conn = withGame games gameId $ connectGame maybeCookies conn

connectGame :: (MonadIO f, MonadError ServerError f) => Maybe Text -> Connection -> Game -> f ()
connectGame maybeCookies conn game = do
  putStrLn "connected"
  player <- case maybeCookies >>= playerIdCookie of
    Just id -> return id
    _ -> do
      liftIO $ putStrLn $ "error: " <> tshow maybeCookies
      throwError err400
  liftIO $ keepAlive conn $ addConnection conn game player

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
    f _ = return $ case maybeCookies >>= playerIdCookie of
      Just _ -> websocketHtml id
      Nothing ->
        form_ [term "hx-post" ("/api/tigris/" <> gameId id <> "/join")] $ do
          input_ [id_ "name", name_ "name", type_ "text"]
          button_ [type_ "submit"] "Join"

websocketHtml :: GameId -> Html ()
websocketHtml id = div_ [id_ "game", term "hx-ext" "ws", term "ws-connect" ("/api/tigris/" <> gameId id <> "/play")] $ div_ [id_ "board"] $ return ()

joinGame :: TVar GameMap -> GameId -> [(Text, Text)] -> Handler JoinGameResponse
joinGame games gameId formData = withGame games gameId $ \game -> case lookup "name" formData of
  Just name -> liftIO $ do
    playerId <- newPlayerId
    atomically $ addPlayer playerId name game
    return $ addPlayerIdCookie playerId $ websocketHtml gameId
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

createNewGame :: TVar GameMap -> PlayerId -> Name -> IO GameId
createNewGame games host name = do
  generatedId <- generateGameId
  game <- atomically $ do
    game <- newGame
    modifyTVar games $ Map.insert generatedId game
    addPlayer host name game
    return game
  forkIO $ hostGame game
  pure generatedId

addPlayer :: PlayerId -> Name -> Game -> STM ()
addPlayer playerId name = flip modifyTVar' (Map.insert playerId name) . playerNames

newGame :: STM Game
newGame = do
  notify <- newTVar []
  ps <- newTVar []
  latestState <- newTVar Map.empty
  names <- newTVar Map.empty
  return Game {latestState = latestState, playerOutputs = notify, playerInputs = ps, waitForFinish = forever $ threadDelay 10000, playerNames = names}

-- Game Joining

-- Add player and setup notification system
addConnection :: Connection -> Game -> PlayerId -> IO ()
addConnection conn game player = do
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
    withAsync (readLoop conn inputQueue) $ \reader ->
      waitForFinish game *> cancel sender *> cancel reader

-- Send updates from the queue to the connection
sendLoop :: TQueue (Map Dynasty (PlayerId, Name)) -> Connection -> PlayerId -> IO ()
sendLoop queue conn player = forever $ do
  state <- atomically $ readTQueue queue
  sendHtml conn $ chooseDynasty state player

chooseDynasty :: Map Dynasty (PlayerId, Name) -> PlayerId -> Html ()
chooseDynasty playerMap player =
  div_ [id_ "board"] $ do
    h2_ "Choose Your Dynasty"
    div_ [class_ "dynasty-grid"]
      $ forM_ [Archer, Bull, Pot, Lion]
      $ dynastyDiv
  where
    dynastyDiv :: Dynasty -> Html ()
    dynastyDiv dynasty = div_ ([class_ "dynasty-box"] ++ if isMine then [class_ "mine"] else []) $ do
      strong_ . toHtml $ show dynasty
      small_ $ toHtml status
      unless isTaken $ button_ [class_ "dynasty", term "hx-vals" jsonVal, term "ws-send" mempty] "Choose"
      where
        (isTaken, isMine, status) = case Map.lookup dynasty playerMap of
          Just (id, name) -> (True, id == player, "Player: " <> name)
          Nothing -> (False, False, "Available")
        jsonVal = encodeToText $ DynastyChoice dynasty

-- Dummy read loop (simulate receiving inputs)
readLoop :: Connection -> TQueue Dynasty -> IO ()
readLoop conn queue = forever $ do
  msg <- receiveData conn
  case decode msg of
    Just (DynastyChoice dynasty) -> do
      putStrLn $ "message: " <> tshow dynasty
      atomically $ writeTQueue queue $ dynasty
    Nothing -> do
      return () -- Handle decoding failure

-- Game Logic Core

hostGame :: Game -> IO ()
hostGame game = do
  playerMap <- seatPlayers game startingState
  playGame playerMap
  where
    startingState = Map.empty

seatPlayers :: Game -> Map Dynasty PlayerId -> IO (Map Dynasty PlayerId)
seatPlayers game = firstNotification >=> setupGameSTM receiveMsg notify
  where
    firstNotification = atomically . returning notify
    notify = withNames >=> notifyPlayers
    withNames m = fmap (`composeMapWithInput` m) . readTVar $ playerNames game
    receiveMsg = (readTVar $ playerInputs game) >>= nextPlayerMessage
    nextPlayerMessage = foldr orElse retry . map (uncurry $ fmap . TakePosition)
    notifyPlayers playerMap = do
      readTVar (playerOutputs game) >>= traverse_ ($ playerMap)
      writeTVar (latestState game) playerMap

composeMapWithInput :: (Ord a) => Map a b -> Map k a -> Map k (a, b)
composeMapWithInput = Map.mapMaybe . withInput . flip Map.lookup
  where
    withInput f a = (a,) <$> f a

notifySetup :: STM [Map Dynasty (PlayerId, Name) -> STM ()] -> Map Dynasty (PlayerId, Name) -> STM ()
notifySetup playersVar playerMap = playersVar >>= traverse_ ($ playerMap)

setupGameSTM :: STM SetupMessage -> (Map Dynasty PlayerId -> STM ()) -> Map Dynasty PlayerId -> IO (Map Dynasty PlayerId)
setupGameSTM receive notify = fixAtomically $ setupGame receive (pure . pure) . notifying
  where
    notifying recurse = returning notify >=> recurse

fixAtomically :: ((a -> STM (IO b)) -> a -> STM (IO b)) -> a -> IO b
fixAtomically f = fix $ \recurse -> join . atomically . (f $ pure . recurse)

setupGame :: (Monad m) => m SetupMessage -> (Map Dynasty PlayerId -> m b) -> (Map Dynasty PlayerId -> m b) -> Map Dynasty PlayerId -> m b
setupGame receive end recurse playerMap = do
  message <- receive
  case message of
    TakePosition player position -> recurse $ takePosition playerMap
      where
        takePosition = if Map.notMember position playerMap then Map.insert position player . Map.filter (/= player) else id
    StartGame -> end playerMap

playGame :: a -> IO ()
playGame = undefined

playerIdKey :: Text
playerIdKey = "playerId"

playerIdCookie :: Text -> Maybe PlayerId
playerIdCookie = fmap PlayerId . getCookie playerIdKey

getCookie :: Text -> Text -> Maybe Text
getCookie key =
  lookup key . parseCookiesText . TE.encodeUtf8

generateId :: IO Text
generateId = stringRandomIO "[a-zA-Z0-9]{5}"

generateGameId :: IO GameId
generateGameId = GameId <$> generateId

newPlayerId :: IO PlayerId
newPlayerId = PlayerId <$> generateId

data DynastyChoice = DynastyChoice {dynasty :: Dynasty} deriving (Generic)

instance ToJSON DynastyChoice

instance FromJSON DynastyChoice
