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

startGameServer :: GameServerDependencies -> IO (Server GameApi)
startGameServer (GameServerDependencies gameKey actions) = actionsApi (responses $ paths gameKey) <$> actions

startServer :: IO (Server Api)
startServer = joinHandlers <$> (startGameServer Tigris.gameServerDependencies)
  where
    joinHandlers tigris = serveGames :<|> serveDirectoryWebApp "static"
      where
        serveGames Tigris = tigris
        serveGames Noughts = undefined

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
            =<< liftIO maybeConnectPlayer
          where
            connect player = connectGame game player conn
            maybeConnectPlayer = do
              names <- tablePlayers game
              traverse connect $ flip Map.lookup names =<< playerIdCookie =<< maybeCookies

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

-- data GameTVars a = GameTVars
--  { latestState :: TVar (Map a (PlayerId, Name)),
--    playerOutputs :: TVar [Map a (PlayerId, Name) -> STM ()],
--    playerInputs :: TVar [(PlayerId, STM (PositionChoice a))],
--    playerNames :: TVar (Map PlayerId Name),
--    waitForFinish :: IO ()
--  }
-- data GameTVars input output = GameTVars
--  { latestState :: TVar output,
--    playerOutputs :: TVar [output -> STM ()],
--    playerInputs :: TVar [(PlayerId, STM input)],
--    playerNames :: TVar (Map PlayerId Name),
--    waitForFinish :: IO ()
--  }

playerIdCookie :: Text -> Maybe PlayerId
playerIdCookie = fmap PlayerId . getCookie playerIdKey

addPlayerIdCookie :: (AddHeader [Optional, Strict] h Text orig new) => Text -> PlayerId -> orig -> new
addPlayerIdCookie path (PlayerId playerId) =
  addHeader (cookieText path playerIdKey playerId)

playerIdKey :: Text
playerIdKey = "playerId"
