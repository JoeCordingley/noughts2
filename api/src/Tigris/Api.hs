{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}


module Tigris.Api (runServer) where

import BasicPrelude
-- import Servant.API.WebSocket (WebSocket)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Function (fix)
import qualified Data.Map as Map
import Lib (recursing)
import Lucid.Base (Html)
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.WebSocket (WebSocket)
import Network.WebSockets.Connection (Connection, receiveData)
import Text.StringRandom (stringRandomIO)
import Web.Cookie (parseCookiesText)
import Control.Concurrent.STM
import Control.Concurrent.Async (withAsync, cancel)
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (..), ToJSON, decode)
import qualified Data.ByteString.Lazy as BL

runServer :: IO ()
runServer = do
  games <- newMVar $ Map.empty
  run 8080 (app games)

type CreateGameResponse = Headers '[Header "HX-Redirect" Text, Header "Set-Cookie" Text] NoContent

data PlayerInput

type API =
  "api"
    :> "tigris"
    :> ( "create"
           :> Post '[JSON] CreateGameResponse
           :<|> Capture "gameId" GameId
             :> Header "Cookie" Text
             :> WebSocket
       )

type GameMap = Map GameId Game
type NotifyPlayer = PlayerMap -> IO ()
data Game  = Game {
  notifyPlayers :: TVar [NotifyPlayer],
  waitForFinish :: IO (),
  players :: TVar [STM SetupMessage]
}


server :: MVar GameMap -> Server API
server games = createGame games :<|> joinGame games

createGame :: MVar GameMap -> Handler CreateGameResponse
createGame games = do
  GameId newId <- liftIO $ createNewGame games
  return $ addHeader ("/games/tigris/" <> newId) $ addHeader (cookie gameCreator newId) NoContent

createNewGame :: MVar GameMap -> IO GameId
createNewGame games = do
  generatedId <- generateId
  game <- newGame
  modifyMVar_ games $ pure . Map.insert generatedId game
  forkIO $ hostGame game
  pure generatedId

newGame :: IO Game
newGame = atomically $ do
  notify <- newTVar []
  ps <- newTVar []
  return Game { notifyPlayers = notify, players = ps, waitForFinish = forever $ threadDelay 10000}

hostGame :: Game -> IO ()
hostGame game = do
  playerMap <- fix (recursing (notifySetup $ notifyPlayers game) . (setupGame $ receiveMsg $ players game)) Bimap.empty
  playGame playerMap

receiveMsg :: TVar [STM SetupMessage] -> IO SetupMessage
receiveMsg players = atomically $ do
  ps <- readTVar players
  foldr orElse retry ps

notifySetup :: TVar [NotifyPlayer] -> PlayerMap -> IO ()
notifySetup playersVar playerMap = do
  players <- atomically $ readTVar playersVar
  traverse_ ($ playerMap) players

playGame :: PlayerMap -> IO ()
playGame = undefined

setupGame :: (Monad m) => m SetupMessage -> (PlayerMap -> m PlayerMap) -> PlayerMap -> m PlayerMap
setupGame receive recurse playerMap = do
  message <- receive
  case message of
    TakePosition player position -> recurse $ takePosition player position playerMap
    --    LeavePosition player -> do
    --      modify $ leavePosition player
    --      recurse
    --    StartGame -> do
    --      n <- modify numberOfPlayers
    --      if n >= 2 then return () else recurse
  where
    takePosition player position playerMap = if unoccupied position playerMap then seat player position playerMap else playerMap
    unoccupied position = Bimap.notMember position
    seat player position = Bimap.insert position player
--    leavePosition = undefined
--    numberOfPlayers = undefined

data SetupMessage
  = TakePosition Player Dynasty deriving (Generic)


--  | LeavePosition player
--  | StartGame

data Player = Player Text deriving (Eq, Ord)

data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic)
instance FromJSON Dynasty

type PlayerMap = Bimap Dynasty Player


gameCreator :: Text
gameCreator = "gameCreator"

cookie :: Text -> Text -> Text
cookie key value = key <> "=" <> value

app :: MVar GameMap -> Application
app games = serve (Proxy :: Proxy API) (server games)

gameCreatorCookie :: Text -> Maybe GameId
gameCreatorCookie text = do
  creatorId <- lookup gameCreator $ parseCookiesText $ encodeUtf8 text
  return $ GameId creatorId

readFromWebSocket :: (BL.ByteString -> Maybe a) -> Connection -> TQueue a -> IO ()
readFromWebSocket decoder conn queue = forever $ do
  msg <- receiveData conn
  traverse (atomically . writeTQueue queue) $ decoder msg

joinGame :: MVar GameMap -> GameId -> Maybe Text -> Server WebSocket
joinGame games gameId maybeCookies conn = do
  gameList <- liftIO $ readMVar games
  case Map.lookup gameId gameList of
    Just game -> liftIO $ addPlayer conn game
    Nothing -> throwError err404
  where
    addPlayer conn game = do 
      player <- newPlayer
      queue <- atomically $ do 
        queue <- newTQueue
        modifyTVar (players game) $ (:) (readTQueue queue)
        return queue
      withAsync (readFromWebSocket (decodeSetupMessage player) conn queue) (\reader -> waitForFinish game *> cancel reader)
      

--  gameList <- liftIO $ readMVar games
--    then pure $ do
--      html_ $ do
--        head_ $ title_ "Tigris Game"
--        body_ $ do
--          h1_ "Welcome to Tigris!"
--          p_ "This is a placeholder for the game."
--          p_ playerParagraph
--    else throwError err404
--  where playerParagraph = case maybeCookies >>= gameCreatorCookie of
--          Just creatorId | creatorId == gameId -> "You are the creator of this game."
--          _ -> "You are joining an existing game."

newPlayer :: IO Player
newPlayer = Player <$> stringRandomIO "[a-zA-Z0-9]{5}"

generateId :: IO GameId
generateId = do
  randomId <- stringRandomIO "[a-zA-Z0-9]{5}"
  pure $ GameId randomId

decodeSetupMessage :: Player -> BL.ByteString -> Maybe SetupMessage 
decodeSetupMessage player = fmap (TakePosition player) . decode 


data GameId = GameId
  { gameId :: Text
  }
  deriving (Eq, Show, Ord)

instance FromHttpApiData GameId where
  parseUrlPiece = Right . GameId
