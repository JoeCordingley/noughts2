{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text (Text)
import Lucid (Attributes, term)
import Lucid.Base (Html, ToHtml (..), renderText)
import Lucid.Html5 (body_, button_, div_, h1_, id_, p_, script_, src_)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.ContentTypes.Lucid

-- Custom HTMX Attributes
hxGet :: Text -> Attributes
hxGet = term "hx-get"

hxTarget :: Text -> Attributes
hxTarget = term "hx-target"

hxSwap :: Text -> Attributes
hxSwap = term "hx-swap"

-- API Definition
type API =
    Get '[HTML] (Html ())
        :<|> "message" :> Get '[HTML] (Html ())

-- Server Implementation
server :: Server API
server =
    pure examplePage
        :<|> pure messageContent

-- Example HTML Page
examplePage :: Html ()
examplePage = do
    body_ $ do
        script_ [src_ "https://unpkg.com/htmx.org@2.0.4"] ""
        h1_ "Welcome to Lucid with HTMX and Servant!"
        p_ "This is a simple example integrating HTMX with Servant and Lucid."
        button_ [hxGet "/message", hxTarget "#message", hxSwap "innerHTML"] "Click me!"
        div_ [id_ "message"] ""

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
