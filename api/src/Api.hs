{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Api (runServer) where

import BasicPrelude
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.STM
import Control.Monad.Error.Class (MonadError)
import Data.Aeson (FromJSON, decode)
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
import Tigris.Api (Dynasty, chooseDynasty)
import Web.Cookie (parseCookiesText)

type Api = "api" :> ("noughts" :> GameApi :<|> "tigris" :> GameApi)

type GameResponse = Html ()

type GameApi =
  "create" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[JSON] CreateGameResponse
    :<|> Capture "gameId" GameId
      :> ( Header "Cookie" Text :> Get '[HTML] GameResponse
             :<|> "join" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[HTML] JoinGameResponse
             :<|> "play" :> Header "Cookie" Text :> WebSocket
         )

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
  { getNoughtsMap :: TVar (GameMap NoughtOrCross),
    getTigrisMap :: TVar (GameMap Dynasty)
  }

data NoughtOrCross = Nought | Cross deriving (Eq, Ord, Show, Generic)

instance FromJSON NoughtOrCross

type GameMap a = Map GameId (Game a)

data Game a = Game
  { latestState :: TVar (Map a (PlayerId, Name)),
    playerOutputs :: TVar [Map a (PlayerId, Name) -> STM ()],
    playerInputs :: TVar [(PlayerId, STM a)],
    playerNames :: TVar (Map PlayerId Name),
    waitForFinish :: IO ()
  }

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
server (GameMaps noughtsMap tigrisMap) = gameserver chooseRoleNoughts (paths "noughts") playNoughts noughtsMap :<|> gameserver chooseDynasty (paths "tigris") playTigris tigrisMap

playNoughts :: Map NoughtOrCross PlayerId -> IO ()
playNoughts playerMap = undefined

playTigris :: Map Dynasty PlayerId -> IO ()
playTigris playerMap = undefined

chooseRoleNoughts :: ChooseRole NoughtOrCross
chooseRoleNoughts = undefined

maybeThrow400 :: (MonadError ServerError f) => (a -> f b) -> Maybe a -> f b
maybeThrow400 = maybe (throwError err400)

gameserver :: (FromJSON a, Show a, Ord a) => ChooseRole a -> Paths -> (Map a PlayerId -> IO ()) -> TVar (GameMap a) -> Server GameApi
gameserver chooseRole (Paths getGamePath getJoinGamePath getPlayPath) playGame games = createGame :<|> gameEndpoints
  where
    createGame = maybeThrow400 (liftIO . createGame') . lookup "name"
    createGame' name = do
      playerId <- newPlayerId
      gameId <- createNewGame playerId
      return $ addHeader (getGamePath gameId) $ addPlayerIdCookie playerId NoContent
      where
        createNewGame host = do
          generatedId <- generateGameId
          game <- atomically $ do
            game <- newGame
            modifyTVar games $ Map.insert generatedId game
            addPlayer host name game
            return game
          forkIO $ hostGame playGame game
          pure generatedId
    gameEndpoints id = gameHandler :<|> joinGame :<|> playGameHandler
      where
        nameForm :: Html ()
        nameForm = form_ [term "hx-post" $ getJoinGamePath id] $ do
          input_ [id_ "name", name_ "name", type_ "text"]
          button_ [type_ "submit"] "Join"
        websocketHtml :: Html ()
        websocketHtml = div_ [id_ "game", term "hx-ext" "ws", term "ws-connect" (getPlayPath id)] $ div_ [id_ "board"] $ return ()
        gameHandler maybeCookies = withGame games id (return . const formOrWebsocket)
          where
            formOrWebsocket = maybe nameForm (const websocketHtml) $ maybeCookies >>= playerIdCookie
        joinGame formData = maybeThrow400 joinGame $ lookup "name" formData
          where
            joinGame name = withGame games id $ liftIO . joinGame
              where
                joinGame game = do
                  playerId <- newPlayerId
                  atomically $ addPlayer playerId name game
                  return $ addPlayerIdCookie playerId websocketHtml
        playGameHandler maybeCookies conn = withGame games id $ connectGame
          where
            connectGame game = do
              putStrLn "connected"
              maybeThrow400 (liftIO . keepAlive conn . addConnection) $ maybeCookies >>= playerIdCookie
              where
                addConnection player = do
                  outputQueue <- newTQueueIO
                  inputQueue <- newTQueueIO
                  atomically $ do
                    modifyTVar' (playerOutputs game) (writeTQueue outputQueue :)
                    modifyTVar' (playerInputs game) ((player, readTQueue inputQueue) :)
                    state <- readTVar (latestState game)
                    writeTQueue outputQueue state
                  withAsync (sendLoop chooseRole outputQueue conn player) $ \sender ->
                    withAsync (readLoop conn inputQueue) $ \reader ->
                      waitForFinish game *> cancel sender *> cancel reader

playerIdCookie :: Text -> Maybe PlayerId
playerIdCookie = fmap PlayerId . getCookie playerIdKey

getCookie :: Text -> Text -> Maybe Text
getCookie key =
  lookup key . parseCookiesText . TE.encodeUtf8

readLoop :: (FromJSON a, Show a) => Connection -> TQueue a -> IO ()
readLoop conn queue = forever $ do
  msg <- receiveData conn
  case decode msg of
    Just (PositionChoice role) -> do
      putStrLn $ "message: " <> tshow role
      atomically $ writeTQueue queue $ role
    Nothing -> do
      return () -- Handle decoding failure

type ChooseRole a = Map a (PlayerId, Name) -> PlayerId -> Html ()

sendLoop :: ChooseRole a -> TQueue (Map a (PlayerId, Name)) -> Connection -> PlayerId -> IO ()
sendLoop chooseRole queue conn player = forever $ do
  state <- atomically $ readTQueue queue
  sendHtml conn $ chooseRole state player

withGame ::
  (MonadIO f, MonadError ServerError f) =>
  TVar (GameMap g) ->
  GameId ->
  (Game g -> f a) ->
  f a
withGame gamesVar gameId action = do
  gameMap <- liftIO $ readTVarIO gamesVar
  maybeThrow400 action $ Map.lookup gameId gameMap

addPlayerIdCookie :: (AddHeader [Optional, Strict] h Text orig new) => PlayerId -> orig -> new
addPlayerIdCookie (PlayerId playerId) = addHeader (cookie playerIdKey playerId)

cookie :: Text -> Text -> Text
cookie key value = key <> "=" <> value

playerIdKey :: Text
playerIdKey = "playerId"

addPlayer :: PlayerId -> Name -> (Game a) -> STM ()
addPlayer playerId name = flip modifyTVar' (Map.insert playerId name) . playerNames

newPlayerId :: IO PlayerId
newPlayerId = PlayerId <$> generateId

generateId :: IO Text
generateId = stringRandomIO "[a-zA-Z0-9]{5}"

generateGameId :: IO GameId
generateGameId = GameId <$> generateId

newGame :: STM (Game a)
newGame = do
  notify <- newTVar []
  ps <- newTVar []
  latestState <- newTVar Map.empty
  names <- newTVar Map.empty
  return Game {latestState = latestState, playerOutputs = notify, playerInputs = ps, waitForFinish = forever $ threadDelay 10000, playerNames = names}

hostGame :: (Ord a) => (Map a PlayerId -> IO ()) -> (Game a) -> IO ()
hostGame playGame game = do
  playerMap <- seatPlayers game startingState
  playGame playerMap
  where
    startingState = Map.empty

seatPlayers :: (Ord a) => Game a -> Map a PlayerId -> IO (Map a PlayerId)
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

setupGameSTM :: (Ord a) => STM (SetupMessage a) -> (Map a PlayerId -> STM ()) -> Map a PlayerId -> IO (Map a PlayerId)
setupGameSTM receive notify = fixAtomically $ setupGame receive (pure . pure) . notifying
  where
    notifying recurse = returning notify >=> recurse

setupGame :: (Monad m, Ord a) => m (SetupMessage a) -> (Map a PlayerId -> m b) -> (Map a PlayerId -> m b) -> Map a PlayerId -> m b
setupGame receive end recurse playerMap = do
  message <- receive
  case message of
    TakePosition player position -> recurse $ takePosition playerMap
      where
        takePosition = if Map.notMember position playerMap then Map.insert position player . Map.filter (/= player) else id
    StartGame -> end playerMap

data SetupMessage a = TakePosition PlayerId a | StartGame deriving (Generic, Show)
