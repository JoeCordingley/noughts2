module Server (runServer) where

import Servant
import Api (Api)
import Lib
import qualified Tigris.Api as Tigris
import Network.Wai.Handler.Warp (run)

runServer :: IO ()
runServer = do
  putStrLn "Running on http://localhost:8081/"
  server <- startServer
  run 8081 (serve (Proxy :: Proxy Api) server)

startServer :: IO (Server Api)
startServer = joinHandlers <$> Tigris.gameServer 
  where
    joinHandlers tigris = serveGames :<|> serveDirectoryWebApp "static"
      where
        serveGames Tigris = tigris
        serveGames Noughts = undefined
        serveGames Chess = undefined
