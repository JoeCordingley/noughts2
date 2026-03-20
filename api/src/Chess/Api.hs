module Chess.Api (gameServer) where

import Servant (Server)
import Api (GameApi)

gameServer :: IO (Server GameApi)
gameServer = undefined
