{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lucid.Base (Html, ToHtml (..), renderText)
import Lucid.Html5 (body_, h1_, p_)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.ContentTypes.Lucid

-- API Definition
type API = Get '[HTML] (Html ())

-- Server Implementation
server :: Server API
server = pure examplePage

-- Example HTML Page
examplePage :: Html ()
examplePage = do
    body_ $ do
        h1_ "Welcome to Lucid2 with Servant!"
        p_ "This is a simple example of using Lucid2 to generate HTML content."

-- Application
app :: Application
app = serve (Proxy :: Proxy API) server

-- Main Function
main :: IO ()
main = do
    putStrLn "Running on http://localhost:8080/"
    run 8080 app
