{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Api (runServer) where

import BasicPrelude hiding (for_)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.STM
import Data.Aeson (FromJSON, decode)
import Data.Composition ((.:))
import qualified Data.Map as Map
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import Lib
import Lucid (term)
import Lucid.Base (Html, termRaw)
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Network.WebSockets.Connection (Connection, receiveData, sendTextData)
import qualified Noughts.Api as Noughts
import Noughts.Game (Game)
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.WebSocket (WebSocket)
import Servant.Links (safeLink)
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import Text.StringRandom (stringRandomIO)
import Tigris.Api (Dynasty)
import qualified Tigris.Api as Tigris
import Web.Cookie (parseCookiesText)

type WithGame = Capture "game" GameKey

type WithGameId = Capture "gameId" GameId

type Api = WithGame :> GameApi :<|> "static" :> Raw

type JoinGameApi = "join" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[HTML] JoinGameResponse

type PlayGameApi = "play" :> Header "Cookie" Text :> WebSocket

type CreateGameApi = ("create" :> Get '[HTML] (Html ()) :<|> CreateGameApiPost)

type CreateGameApiPost = "create" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[JSON] CreateGameResponse

type GameApi =
  ( CreateGameApi
      :<|> ( WithGameId
               :> ( GetGameApi
                      :<|> JoinGameApi
                      :<|> PlayGameApi
                  )
           )
  )

type GetGameApi = Header "Cookie" Text :> Get '[HTML] GameResponse

type GameResponse = Html ()

type CreateGameResponse = Headers '[Header "HX-Redirect" Text, Header "Set-Cookie" Text] NoContent

type JoinGameResponse = Headers '[Header "Set-Cookie" Text] (Html ())

runServer :: IO ()
runServer = do
  putStrLn "Running on http://localhost:8080/"
  server <- startServer
  run 8080 (serve (Proxy :: Proxy Api) server)

data Paths = Paths
  { getGamePath :: GameId -> Text,
    getJoinGamePath :: GameId -> Text,
    getPlayPath :: GameId -> Text,
    createGameApi :: Text
  }

paths :: GameKey -> Paths
paths game =
  Paths
    { getGamePath = rootUrl . toUrlPiece . apiLink (Proxy @(WithGame :> WithGameId :> GetGameApi)),
      getJoinGamePath = rootUrl . toUrlPiece . apiLink (Proxy @(WithGame :> WithGameId :> JoinGameApi)),
      getPlayPath = rootUrl . toUrlPiece . apiLink (Proxy @(WithGame :> WithGameId :> PlayGameApi)),
      createGameApi = rootUrl . toUrlPiece $ apiLink (Proxy @(WithGame :> CreateGameApiPost))
    }
  where
    rootUrl = ("/" <>)
    apiLink api = safeLink (Proxy @Api) api game

startGameServer :: (FromJSON a, Ord a, Show a) => GameServerDependencies a -> IO (Server GameApi)
startGameServer (GameServerDependencies {gameKey, playGame, isReady, chooseRole}) = actionsApi (responses $ paths gameKey) . actions hostGame chooseRole <$> newTVarIO Map.empty
  where
    hostGame = seatPlayers isReady >=> playGame

actions :: (FromJSON a, Show a) => (GameTVars a -> IO ()) -> ChooseRoleHtml a -> TVar (Map GameId (GameTVars a)) -> Actions
actions hostGame chooseRole gameMap = Actions {createGame, newPlayerId, getGame}
  where
    newPlayerId = PlayerId <$> generateId
    createGame player = do
      gameId <- generateGameId
      game <- atomically $ returning (modifyTVar gameMap . Map.insert gameId) =<< newGame player
      forkIO $ hostGame game
      pure gameId
    getGame gameId = (fmap (table chooseRole) . Map.lookup gameId) <$> readTVarIO gameMap

startServer :: IO (Server Api)
startServer = joinHandlers <$> (startGameServer Noughts.gameServerDependencies) <*> (startGameServer Tigris.gameServerDependencies)
  where
    joinHandlers noughts tigris = serveGames :<|> serveDirectoryWebApp "static"
      where
        serveGames Tigris = tigris
        serveGames Noughts = noughts

seatPlayers :: (Ord a) => (Map a PlayerId -> Bool) -> GameTVars a -> IO (Map a PlayerId)
seatPlayers isReady game = setupGameSTM startingState
  where
    receive = receiveMsg $ playerInputs game
    setupGameSTM = updateWithNotify (setupGame isReady receive (pure . pure)) notify
    startingState = Map.empty
    firstNotification = atomically . returning notify
    notify = withNames >=> notifyPlayers
    withNames m = fmap (`composeMapWithInput` m) . readTVar $ playerNames game
    notifyPlayers playerMap = do
      readTVar (playerOutputs game) >>= traverse_ ($ playerMap)
      writeTVar (latestState game) playerMap

newGame :: Player -> STM (GameTVars a)
newGame (Player {playerId, playerName}) = do
  notify <- newTVar []
  ps <- newTVar []
  latestState <- newTVar Map.empty
  names <- newTVar $ Map.singleton playerId playerName
  return GameTVars {latestState = latestState, playerOutputs = notify, playerInputs = ps, waitForFinish = forever $ threadDelay 10000, playerNames = names}

receiveMsg :: TVar [(PlayerId, STM (PositionChoice a))] -> STM (SetupMessage a)
receiveMsg = readTVar >=> nextPlayerMessage
  where
    nextPlayerMessage = foldr orElse retry . map (uncurry $ fmap . setupMessage)

data Actions = Actions {createGame :: Player -> IO GameId, newPlayerId :: IO PlayerId, getGame :: GameId -> IO (Maybe Table)}

actionsApi :: Responses -> Actions -> Server GameApi
actionsApi (Responses {createGamePage, createGameResponse, knownPlayerResponse, unknownPlayerResponse, joinGameResponse}) (Actions {createGame, newPlayerId, getGame}) = (gameHomeHandler :<|> createGameHandler) :<|> gameEndpoints
  where
    gameHomeHandler = return createGamePage
    createGameHandler = maybe (throwError err400) (liftIO . handleCreateGame) . lookup "name"
      where
        handleCreateGame name = do
          playerId <- newPlayerId
          putStrLn $ "new player id: " <> tshow playerId
          gameId <- createGame $ Player playerId name
          return $ createGameResponse gameId playerId
    gameEndpoints id = withGame . const . gameHandler :<|> withGame . joinGameHandler :<|> withGame .: playGameHandler
      where
        gameHandler maybeCookies = do
          return . bool (unknownPlayerResponse id) (knownPlayerResponse id) . isJust $ playerIdCookie =<< maybeCookies
        withGame f = maybe (throwError err400) f =<< (liftIO $ getGame id)
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
            =<< liftIO
              ( do
                  putStrLn $ "cookies: " <> tshow maybeCookies
                  traverse connect $ maybeCookies >>= playerIdCookie
              )
          where
            connect playerId = connectGame game playerId conn

data Player = Player {playerId :: PlayerId, playerName :: Name}

data Responses = Responses
  { createGameResponse :: GameId -> PlayerId -> CreateGameResponse,
    knownPlayerResponse :: GameId -> GameResponse,
    unknownPlayerResponse :: GameId -> GameResponse,
    joinGameResponse :: GameId -> PlayerId -> JoinGameResponse,
    createGamePage :: Html ()
  }

htmxPage :: Html () -> Html ()
htmxPage content = html_ $ do
  head_ $ do
    title_ "Tigers and Pots"
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    script_ [src_ "https://unpkg.com/htmx.org@2.0.4"] ("" :: Text)
    link_ [rel_ "stylesheet", href_ "/static/css/dynasty.css"]
    script_ [src_ "https://unpkg.com/htmx.org@2.0.4/dist/ext/ws.js"] ("" :: Text)
  body_ content

responses :: Paths -> Responses
responses (Paths {getGamePath, getPlayPath, getJoinGamePath, createGameApi}) = Responses {createGameResponse, knownPlayerResponse, unknownPlayerResponse, joinGameResponse, createGamePage}
  where
    createGamePage :: Html ()
    createGamePage = htmxPage $ div_ [] $ do
      h1_ "Tigers and Pots"
      form_ [term "hx-post" createGameApi, term "hx-target" "body"] $ do
        label_ [for_ "player-name"] "Your name :"
        input_ [id_ "name", name_ "name", type_ "text", term "required" ""]
        button_ [type_ "submit"] "Create Game"
    createGameResponse gameId playerId =
      addHeader (getGamePath gameId) $ addPlayerIdCookie (getGamePath gameId) playerId NoContent
    websocketDiv :: GameId -> Html ()
    websocketDiv id = div_ [id_ "game", term "hx-ext" "ws", term "ws-connect" (getPlayPath id)] $ div_ [id_ "board"] $ return ()
    knownPlayerResponse = htmxPage . websocketDiv
    unknownPlayerResponse :: GameId -> Html ()
    unknownPlayerResponse id = htmxPage $ form_ [term "hx-post" $ getJoinGamePath id] $ do
      input_ [id_ "name", name_ "name", type_ "text"]
      button_ [type_ "submit"] "Join"
    joinGameResponse gameId playerId = addPlayerIdCookie (getGamePath gameId) playerId (websocketDiv gameId)

table :: (FromJSON a, Show a) => ChooseRoleHtml a -> GameTVars a -> Table
table chooseRole tvars = Table {addPlayer, connectGame}
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

data Table = Table {addPlayer :: Player -> IO (), connectGame :: PlayerId -> Connection -> IO ()}

playerIdCookie :: Text -> Maybe PlayerId
playerIdCookie = fmap PlayerId . getCookie playerIdKey

readLoop :: (FromJSON a, Show a) => Connection -> TQueue (PositionChoice a) -> IO ()
readLoop conn queue = forever $ do
  msg <- receiveData conn
  putStrLn $ "msg: " <> tshow msg
  case decode msg of
    Just message -> do
      putStrLn $ "message: " <> tshow message
      atomically $ writeTQueue queue $ message
    Nothing -> do
      return () -- Handle decoding failure

sendLoop :: (Show a) => ChooseRoleHtml a -> TQueue (Map a (PlayerId, Name)) -> Connection -> PlayerId -> IO ()
sendLoop chooseRole queue conn player = forever $ do
  state <- atomically $ readTQueue queue
  putStrLn $ "sending" <> tshow state
  sendHtml conn $ chooseRole state player

addPlayerIdCookie :: (AddHeader [Optional, Strict] h Text orig new) => Text -> PlayerId -> orig -> new
addPlayerIdCookie path (PlayerId playerId) =
  addHeader (cookieText path playerIdKey playerId)

playerIdKey :: Text
playerIdKey = "playerId"

addPlayerSTM :: PlayerId -> Name -> (GameTVars a) -> STM ()
addPlayerSTM playerId name = flip modifyTVar' (Map.insert playerId name) . playerNames

generateId :: IO Text
generateId = stringRandomIO "[a-zA-Z0-9]{5}"

generateGameId :: IO GameId
generateGameId = GameId <$> generateId

updateWithNotify :: ((a -> STM (IO b)) -> a -> STM (IO b)) -> (a -> STM ()) -> a -> IO b
updateWithNotify update notify = (fixAtomically $ update . notifying) <=< returning (atomically . notify)
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
