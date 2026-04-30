{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Api (Api, GameApi, actionsApi, paths, responses) where

import BasicPrelude hiding (for_)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Composition ((.:))
import qualified Data.Text.Encoding as TE
import Lib
import Lucid (term)
import Lucid.Base (Html)
import Lucid.Html5
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.WebSocket (WebSocket)
import Web.Cookie


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


actionsApi :: Responses -> Actions -> Server GameApi
actionsApi (Responses {createGamePage, createGameResponse, knownPlayerResponse, unknownPlayerResponse, joinGameResponse}) (Actions {createGame, newPlayerId, getGame}) = (gameHomeHandler :<|> createGameHandler) :<|> gameEndpoints
  where
    gameHomeHandler = return createGamePage
    createGameHandler = maybe (throwError err400) (liftIO . handleCreateGame) . lookup "name"
      where
        handleCreateGame name = do
          playerId <- newPlayerId
          gameId <- createGame $ Player playerId name
          return $ createGameResponse gameId playerId
    gameEndpoints id' = withGame . const . gameHandler :<|> withGame . joinGameHandler :<|> withGame .: playGameHandler
      where
        gameHandler maybeCookies = do
          return . bool (unknownPlayerResponse id') (knownPlayerResponse id') . isJust $ playerIdCookie =<< maybeCookies
        withGame f = maybe (throwError err400) f =<< (liftIO $ getGame id')
        joinGameHandler formData game =
          maybe (throwError err400) (return . joinGameResponse id')
            =<< liftIO (traverse joinGame $ lookup "name" formData)
          where
            joinGame name = returning addPlayer' =<< newPlayerId where
              addPlayer' playerId = addPlayer game (Player playerId name)
        playGameHandler maybeCookies conn game =
          maybe (throwError err400) return
            =<< liftIO maybeConnectPlayer
          where
            connect player = connectGame game player conn
            maybeConnectPlayer = traverse connect . join =<< traverse (tablePlayer game) (playerIdCookie =<< maybeCookies)

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
    script_ [src_ "https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js"] ("" :: Text)
    script_ [src_ "https://cdn.jsdelivr.net/npm/htmx-ext-ws@2.0.4"] ("" :: Text)
    script_ [defer_ "", src_ "https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"] ("" :: Text)
    link_ [rel_ "stylesheet", href_ "/static/css/dynasty.css?v=2"]
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
    websocketDiv id' = div_ [term "hx-ext" "ws", term "ws-connect" (getPlayPath id')] $ gameDiv $ return ()
    knownPlayerResponse = htmxPage . websocketDiv
    unknownPlayerResponse :: GameId -> Html ()
    unknownPlayerResponse id' = htmxPage $ form_ [term "hx-post" $ getJoinGamePath id'] $ do
      input_ [id_ "name", name_ "name", type_ "text"]
      button_ [type_ "submit"] "Join"
    joinGameResponse gameId playerId = addPlayerIdCookie (getGamePath gameId) playerId (websocketDiv gameId)

playerIdCookie :: Text -> Maybe PlayerId
playerIdCookie = fmap PlayerId . getCookie playerIdKey

cookieText :: Text -> Text -> Text -> Text
cookieText path key value =
  decodeUtf8
    . BL.toStrict
    . BB.toLazyByteString
    . renderSetCookie
    $ defaultSetCookie
      { setCookieName = encodeUtf8 key,
        setCookieValue = encodeUtf8 value,
        setCookiePath = Just $ encodeUtf8 path,
        setCookieHttpOnly = True,
        setCookieSameSite = Just sameSiteLax
      }

addPlayerIdCookie :: (AddHeader [Optional, Strict] h Text orig new) => Text -> PlayerId -> orig -> new
addPlayerIdCookie path (PlayerId playerId) =
  addHeader (cookieText path playerIdKey playerId)

playerIdKey :: Text
playerIdKey = "playerId"

getCookie :: Text -> Text -> Maybe Text
getCookie key =
  lookup key . parseCookiesText . TE.encodeUtf8
