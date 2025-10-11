{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Api (runServer) where

import BasicPrelude
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.STM
import Data.Aeson (FromJSON, decode)
import Data.Composition ((.:))
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import Lib
import Lucid (term)
import Lucid.Base (Html)
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Network.WebSockets.Connection (Connection, receiveData)
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.WebSocket (WebSocket)
import Text.StringRandom (stringRandomIO)
import Tigris.Api (Dynasty)
import qualified Tigris.Api as Tigris
import Web.Cookie (parseCookiesText)

type Api = "api" :> ("noughts" :> GameApi :<|> "tigris" :> GameApi)

type GameApi =
  "create" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[JSON] CreateGameResponse
    :<|> Capture "gameId" GameId
      :> ( Header "Cookie" Text :> Get '[HTML] GameResponse
             :<|> "join" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[HTML] JoinGameResponse
             :<|> "play" :> Header "Cookie" Text :> WebSocket
         )

type GameResponse = Html ()

type CreateGameResponse = Headers '[Header "HX-Redirect" Text, Header "Set-Cookie" Text] NoContent

type JoinGameResponse = Headers '[Header "Set-Cookie" Text] (Html ())

runServer :: IO ()
runServer = do
  gameMaps <- newGameMaps
  putStrLn "Running on http://localhost:8080/"
  run 8080 (serve (Proxy :: Proxy Api) (server gameMaps))

newGameMaps :: IO GameMaps
newGameMaps =
  GameMaps
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty

data GameMaps = GameMaps
  { noughtsMap :: TVar (GameMap NoughtOrCross),
    tigrisMap :: TVar (GameMap Dynasty)
  }

data NoughtOrCross = Nought | Cross deriving (Eq, Ord, Show, Generic)

instance FromJSON NoughtOrCross

type GameMap a = Map GameId (GameTVars a)

data Paths = Paths
  { getGamePath :: GameId -> Text,
    getJoinGamePath :: GameId -> Text,
    getPlayPath :: GameId -> Text
  }

paths :: Text -> Paths
paths game =
  Paths
    { getGamePath = \id -> "/games/" <> game <> "/" <> gameId id,
      getJoinGamePath = \id -> "/api/" <> game <> "/" <> gameId id <> "/join",
      getPlayPath = \id -> "/api/" <> game <> "/" <> gameId id <> "/play"
    }

server :: GameMaps -> Server Api
server (GameMaps {noughtsMap, tigrisMap}) = gameserver' (paths "noughts") noughtsMap playNoughts isReadyNoughts chooseRoleNoughts :<|> gameserver' (paths "tigris") tigrisMap playTigris Tigris.isReady Tigris.chooseDynasty
  where
    gameserver' paths gameMap playGame isReady chooseRole = gameserver (responses paths) (createNewGame gameMap newGame (seatPlayers isReady >=> playGame)) newPlayerId ((fmap . fmap) (gameFromTVars chooseRole) . getGame gameMap) where

getGame :: TVar (GameMap a) -> GameId -> IO (Maybe (GameTVars a))
getGame gameMap gameId = Map.lookup gameId <$> readTVarIO gameMap

playNoughts :: Map NoughtOrCross PlayerId -> IO ()
playNoughts playerMap = undefined

playTigris :: Map Dynasty PlayerId -> IO ()
playTigris playerMap = undefined

chooseRoleNoughts :: ChooseRoleHtml NoughtOrCross
chooseRoleNoughts = undefined

isReadyNoughts :: Map NoughtOrCross PlayerId -> Bool
isReadyNoughts playerMap = Map.size playerMap >= 2

nextPlayerMessage :: [(PlayerId, STM (PositionChoice a))] -> STM (SetupMessage a)
nextPlayerMessage = foldr orElse retry . map (uncurry $ fmap . setupMessage)

receiveMsg :: TVar [(PlayerId, STM (PositionChoice a))] -> STM (SetupMessage a)
receiveMsg = readTVar >=> nextPlayerMessage

createNewGame :: TVar (GameMap a) -> (Player -> STM (GameTVars a)) -> (GameTVars a -> IO ()) -> Player -> IO GameId
createNewGame games newGame hostGame player = do
  gameId <- generateGameId
  game <- atomically $ do
    game <- newGame player
    modifyTVar games $ Map.insert gameId game
    return game
  forkIO $ hostGame game
  pure gameId

seatPlayers :: (Ord a) => (Map a PlayerId -> Bool) -> GameTVars a -> IO (Map a PlayerId)
seatPlayers isReady game = firstNotification startingState >>= setupGameSTM isReady (receiveMsg $ playerInputs game) notify
  where
    startingState = Map.empty
    firstNotification = atomically . returning notify
    notify = withNames >=> notifyPlayers
    withNames m = fmap (`composeMapWithInput` m) . readTVar $ playerNames game
    notifyPlayers playerMap = do
      readTVar (playerOutputs game) >>= traverse_ ($ playerMap)
      writeTVar (latestState game) playerMap

gameserver :: Responses -> (Player -> IO GameId) -> IO PlayerId -> (GameId -> IO (Maybe Game)) -> Server GameApi
gameserver (Responses createGameResponse websocketResponse formResponse joinGameResponse) createGame newPlayerId getGame = createGameHandler :<|> gameEndpoints
  where
    createGameHandler = maybe (throwError err400) (liftIO . handleCreateGame) . lookup "name"
      where
        handleCreateGame name = do
          playerId <- newPlayerId
          gameId <- createGame $ Player playerId name
          return $ createGameResponse gameId playerId
    gameEndpoints id = wg . gameHandler :<|> wg . joinGameHandler :<|> wg .: playGameHandler
      where
        wg f = maybe (throwError err400) f =<< (liftIO $ getGame id)
        gameHandler maybeCookies game = return formOrWebsocket
          where
            formOrWebsocket = bool (websocketResponse id) (formResponse id) . isJust $ playerIdCookie =<< maybeCookies
        joinGameHandler formData game =
          maybe (throwError err400) (return . joinGameResponse id)
            =<< liftIO (traverse joinGame $ lookup "name" formData)
          where
            joinGame name = do
              playerId <- newPlayerId
              addPlayer game (Player playerId name)
              return playerId
        playGameHandler maybeCookies conn game =
          maybe (throwError err400) return
            =<< liftIO (traverse connect $ maybeCookies >>= playerIdCookie)
          where
            connect playerId = connectGame game playerId conn

data Player = Player {playerId :: PlayerId, playerName :: Name}

data Responses = Responses
  { createGameResponse :: GameId -> PlayerId -> CreateGameResponse,
    websocketResponse :: GameId -> GameResponse,
    formResponse :: GameId -> GameResponse,
    joinGameResponse :: GameId -> PlayerId -> JoinGameResponse
  }

responses :: Paths -> Responses
responses (Paths {getGamePath, getPlayPath, getJoinGamePath}) = Responses {createGameResponse, websocketResponse, formResponse, joinGameResponse}
  where
    createGameResponse gameId playerId = addHeader (getGamePath gameId) $ addPlayerIdCookie playerId NoContent
    websocketResponse :: GameId -> Html ()
    websocketResponse id = div_ [id_ "game", term "hx-ext" "ws", term "ws-connect" (getPlayPath id)] $ div_ [id_ "board"] $ return ()
    formResponse :: GameId -> Html ()
    formResponse id = form_ [term "hx-post" $ getJoinGamePath id] $ do
      input_ [id_ "name", name_ "name", type_ "text"]
      button_ [type_ "submit"] "Join"
    joinGameResponse gameId playerId = addPlayerIdCookie playerId (websocketResponse gameId)

gameFromTVars :: (FromJSON a, Show a) => ChooseRoleHtml a -> GameTVars a -> Game
gameFromTVars chooseRole tvars = Game {addPlayer, connectGame}
  where
    addPlayer (Player {playerId, playerName}) = atomically $ addPlayerSTM playerId playerName tvars
    connectGame playerId connection = do
      putStrLn "connected"
      outputQueue <- newTQueueIO
      inputQueue <- newTQueueIO
      atomically $ do
        modifyTVar' (playerOutputs tvars) (writeTQueue outputQueue :)
        modifyTVar' (playerInputs tvars) ((playerId, readTQueue inputQueue) :)
        state <- readTVar (latestState tvars)
        writeTQueue outputQueue state
      withAsync (sendLoop chooseRole outputQueue connection playerId) $ \sender ->
        withAsync (readLoop connection inputQueue) $ \reader ->
          waitForFinish tvars *> cancel sender *> cancel reader

data GameTVars a = GameTVars
  { latestState :: TVar (Map a (PlayerId, Name)),
    playerOutputs :: TVar [Map a (PlayerId, Name) -> STM ()],
    playerInputs :: TVar [(PlayerId, STM (PositionChoice a))],
    playerNames :: TVar (Map PlayerId Name),
    waitForFinish :: IO ()
  }

data Game = Game {addPlayer :: Player -> IO (), connectGame :: PlayerId -> Connection -> IO ()}

playerIdCookie :: Text -> Maybe PlayerId
playerIdCookie = fmap PlayerId . getCookie playerIdKey

getCookie :: Text -> Text -> Maybe Text
getCookie key =
  lookup key . parseCookiesText . TE.encodeUtf8

readLoop :: (FromJSON a, Show a) => Connection -> TQueue (PositionChoice a) -> IO ()
readLoop conn queue = forever $ do
  msg <- receiveData conn
  case decode msg of
    Just message -> do
      putStrLn $ "message: " <> tshow message
      atomically $ writeTQueue queue $ message
    Nothing -> do
      return () -- Handle decoding failure

type ChooseRoleHtml a = Map a (PlayerId, Name) -> PlayerId -> Html ()

sendLoop :: ChooseRoleHtml a -> TQueue (Map a (PlayerId, Name)) -> Connection -> PlayerId -> IO ()
sendLoop chooseRole queue conn player = forever $ do
  state <- atomically $ readTQueue queue
  sendHtml conn $ chooseRole state player

addPlayerIdCookie :: (AddHeader [Optional, Strict] h Text orig new) => PlayerId -> orig -> new
addPlayerIdCookie (PlayerId playerId) = addHeader (cookie playerIdKey playerId)

cookie :: Text -> Text -> Text
cookie key value = key <> "=" <> value

playerIdKey :: Text
playerIdKey = "playerId"

addPlayerSTM :: PlayerId -> Name -> (GameTVars a) -> STM ()
addPlayerSTM playerId name = flip modifyTVar' (Map.insert playerId name) . playerNames

newPlayerId :: IO PlayerId
newPlayerId = PlayerId <$> generateId

generateId :: IO Text
generateId = stringRandomIO "[a-zA-Z0-9]{5}"

generateGameId :: IO GameId
generateGameId = GameId <$> generateId

newGame :: Player -> STM (GameTVars a)
newGame (Player {playerId, playerName}) = do
  notify <- newTVar []
  ps <- newTVar []
  latestState <- newTVar Map.empty
  names <- newTVar $ Map.singleton playerId playerName
  return GameTVars {latestState = latestState, playerOutputs = notify, playerInputs = ps, waitForFinish = forever $ threadDelay 10000, playerNames = names}

setupGameSTM :: (Ord a) => (Map a PlayerId -> Bool) -> STM (SetupMessage a) -> (Map a PlayerId -> STM ()) -> Map a PlayerId -> IO (Map a PlayerId)
setupGameSTM isReady receive notify = fixAtomically $ setupGame isReady receive (pure . pure) . notifying
  where
    notifying recurse = returning notify >=> recurse

setupGame :: (Monad m, Ord a) => (Map a PlayerId -> Bool) -> m (SetupMessage a) -> (Map a PlayerId -> m b) -> (Map a PlayerId -> m b) -> Map a PlayerId -> m b
setupGame isReady receive end recurse playerMap = do
  message <- receive
  case message of
    TakePosition player position -> recurse $ takePosition playerMap
      where
        takePosition = if Map.notMember position playerMap then Map.insert position player . Map.filter (/= player) else id
    Start -> (if isReady playerMap then end else recurse) playerMap

setupMessage :: PlayerId -> PositionChoice a -> SetupMessage a
setupMessage playerId (PositionChoice a) = TakePosition playerId a
setupMessage _ StartGame = Start

data SetupMessage a = TakePosition PlayerId a | Start deriving (Generic, Show)
