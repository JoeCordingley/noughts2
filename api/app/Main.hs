{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Control.Monad (forever, (<=<))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)

-- import Lucid (Attributes, term)
import Lucid.Base (Html)
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Network.WebSockets.Connection (Connection, receiveData, withPingThread)
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.WebSocket (WebSocket)

-- Custom HTMX Attributes
-- hxGet :: Text -> Attributes
-- hxGet = term "hx-get"
--
-- hxTarget :: Text -> Attributes
-- hxTarget = term "hx-target"
--
-- hxSwap :: Text -> Attributes
-- hxSwap = term "hx-swap"
--
-- hxExt :: Text -> Attributes
-- hxExt = term "hx-ext"
--
-- wsConnect :: Text -> Attributes
-- wsConnect = term "ws-connect"

-- wsSend :: Attributes
-- wsSend = term "ws-send" mempty

-- hxVals :: Text -> Attributes
-- hxVals = term "hx-vals"

-- API Definition
type API =
    Get '[HTML] (Html ())
        :<|> "message" :> Get '[HTML] (Html ())
        :<|> "join" :> WebSocket

-- Server Implementation
server :: Server API
server =
    pure examplePage
        :<|> pure messageContent
        :<|> websocketsApp

websocketsApp :: Server WebSocket
websocketsApp conn = keepAlive conn communication
  where
    communication :: ExceptT ServerError IO ()
    communication = liftIO $ forever $ receiveData conn >>= Text.putStrLn

keepAlive :: (MonadError e m, MonadIO m) => Connection -> ExceptT e IO c -> m c
keepAlive conn =
    liftEither <=< liftIO . withPingThread conn 30 (pure ()) . runExceptT

-- Example HTML Page
examplePage :: Html ()
examplePage = do
    head_ $ do
        title_ "Noughts and Crosses"
        script_ [src_ "https://unpkg.com/htmx.org@2.0.4"] ""
        script_ [src_ "https://unpkg.com/htmx-ext-ws@2.0.1/ws.js"] ""
    body_ $ do
        h1_ "Noughts and Crosses"

-- HTMX Response Content
messageContent :: Html ()
messageContent = p_ "Hello from the server! This was loaded via HTMX."

-- Application
app :: Application
app = serve (Proxy :: Proxy API) server

-- Main Function
main :: IO ()
main = do
    putStrLn "Running on http://localhost:8080/"
    run 8080 app
