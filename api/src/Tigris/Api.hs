{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tigris.Api (runServer) where

import BasicPrelude
-- import Servant.API.WebSocket (WebSocket)

import Lucid.Base (Html)
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.ContentTypes.Lucid

runServer :: IO ()
runServer = run 8080 app

type API =
  "api"
    :> "tigris"
    :> ( "create"
           :> Post '[JSON] (Headers '[Header "HX-Redirect" Text] NoContent)
           :<|> Capture "gameId" GameId
             :> Get '[HTML] (Html ())
       )

app :: Application
app = serve (Proxy :: Proxy API) server

server :: Server API
server = createGame :<|> joinGame

createGame :: Handler (Headers '[Header "HX-Redirect" Text] NoContent)
createGame = do
  putStrLn "Creating a new game..."
  newId <- liftIO $ createNewGame
  return $ addHeader ("/games/tigris/" <> newId) NoContent

stubGameId :: Text
stubGameId = "stub-game-id"

joinGame :: GameId -> Handler (Html ())
joinGame (GameId thisGame) =
  if thisGame == stubGameId
    then pure $ do
      html_ $ do
        head_ $ title_ "Tigris Game"
        body_ $ do
          h1_ "Welcome to Tigris!"
          p_ "This is a placeholder for the game."
    else throwError err404

createNewGame :: IO Text
createNewGame = pure stubGameId

data GameId = GameId
  { gameId :: Text
  }
  deriving (Eq, Show)

instance FromHttpApiData GameId where
  parseUrlPiece = Right . GameId
