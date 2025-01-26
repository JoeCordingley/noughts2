{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (run)
import Servant

-- Define a custom HTML content type
data HTML

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
    mimeRender _ = TL.encodeUtf8 . TL.fromStrict

-- Define a custom HTML content type
data HTMLFile

instance Accept HTMLFile where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTMLFile BL.ByteString where
    mimeRender _ = id

-- Define the API type
type API =
    "api" :> "message" :> Post '[HTML] Text :<|> "home" :> Get '[HTMLFile] BL.ByteString

-- Implement the server
server :: Server API
server = do
    htmlContent <- liftIO $ BL.readFile "static/index.html"
    return "<p>Hello from the Servant API!</p>" :<|> return htmlContent

-- Proxy for the API
api :: Proxy API
api = Proxy

-- Main entry point
main :: IO ()
main = do
    putStrLn "Starting Servant server on http://localhost:8080"
    run 8080 $ serve api server
