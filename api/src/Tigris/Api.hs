{-# LANGUAGE NoImplicitPrelude #-}

module Tigris.Api (runServer) where

import BasicPrelude


runServer :: IO ()
runServer = undefined

type API = "tigris" :> "create" :> Post '[JSON] GameId
      :<|> "tigris" :> "join" :> Capture "gameId" GameId :> WebSocket

