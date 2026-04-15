module Chess.Api (gameServer) where

import Servant (Server)
import Lib

import Api (GameApi, actionsApi, responses, paths)

gameServer :: IO (Server GameApi)
gameServer = actionsApi (responses $ paths Chess) <$> chessActions

chessActions :: IO Actions
chessActions = undefined

--openingState :: (Maybe Player, Maybe Player)
--openingState = (Nothing, Nothing)
