{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Tigris.Api (runServer) where

import BasicPrelude
-- import Servant.API.WebSocket (WebSocket)

import Lucid.Base (Html)
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.ContentTypes.Lucid
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Control.Concurrent (forkIO)
import Text.StringRandom (stringRandomIO)
import Web.Cookie (parseCookiesText)
import qualified Data.Bimap as Bimap
import Data.Bimap (Bimap)
import Data.Function (fix)
import Lib (recursing)
import qualified Data.Map as Map

runServer :: IO ()
runServer = do
  games <- newMVar $ Map.empty
  run 8080 (app games)

type CreateGameResponse = Headers '[Header "HX-Redirect" Text, Header "Set-Cookie" Text] NoContent
type API =
  "api"
    :> "tigris"
    :> ( "create"
           :> Post '[JSON] CreateGameResponse
           :<|> Capture "gameId" GameId
             :> Header "Cookie" Text
             :> Get '[HTML] (Html ())
       )


type Games = MVar (Map GameId (MVar [PlayerMap -> IO ()]))

server :: Games -> Server API
server games = createGame games :<|> joinGame games

createGame :: Games -> Handler CreateGameResponse
createGame games = do
  GameId newId <- liftIO $ createNewGame games
  return $ addHeader ("/games/tigris/" <> newId) $ addHeader (cookie gameCreator newId) NoContent

createNewGame :: Games -> IO GameId
createNewGame games = do
  generatedId <- generateId
  playersVar <- newMVar []
  modifyMVar_ games $ pure . Map.insert generatedId playersVar
  forkIO $ hostGame playersVar
  pure generatedId

hostGame :: MVar [PlayerMap -> IO ()] -> IO ()
hostGame playersVar = do
  playerMap <- fix (recursing (notifySetup playersVar) . setupGame receiveMsg) Bimap.empty
  playGame playerMap

receiveMsg :: IO SetupMessage
receiveMsg = undefined

notifySetup :: MVar [PlayerMap -> IO ()] -> PlayerMap -> IO ()
notifySetup playersVar playerMap = do
  players <- readMVar playersVar
  traverse_ ($ playerMap) players

playGame :: PlayerMap -> IO ()
playGame = undefined

setupGame :: Monad m => m SetupMessage -> (PlayerMap -> m PlayerMap) -> PlayerMap -> m PlayerMap
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
  = TakePosition Player Dynasty
--  | LeavePosition player
--  | StartGame

data Player = Player String deriving (Eq, Ord)
data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord)

type PlayerMap = Bimap Dynasty Player
type ModifyPlayers m = forall a. (PlayerMap -> (a, PlayerMap)) -> m a

gameCreator :: Text
gameCreator = "gameCreator"

cookie :: Text -> Text -> Text
cookie key value = key <> "=" <> value

app :: Games -> Application
app games = serve (Proxy :: Proxy API) (server games)

gameCreatorCookie :: Text -> Maybe GameId
gameCreatorCookie text = do
  creatorId <- lookup gameCreator $ parseCookiesText $ encodeUtf8 text
  return $ GameId creatorId

joinGame :: Games -> GameId -> Maybe Text -> Handler (Html ())
joinGame games gameId maybeCookies = do
  gameList <- liftIO $ readMVar games
  if Map.member gameId gameList
    then pure $ do
      html_ $ do
        head_ $ title_ "Tigris Game"
        body_ $ do
          h1_ "Welcome to Tigris!"
          p_ "This is a placeholder for the game."
          p_ playerParagraph
    else throwError err404
  where playerParagraph = case maybeCookies >>= gameCreatorCookie of
          Just creatorId | creatorId == gameId -> "You are the creator of this game."
          _ -> "You are joining an existing game."

generateId :: IO GameId
generateId = do
  randomId <- stringRandomIO "[a-zA-Z0-9]{5}" 
  pure $ GameId randomId


data GameId = GameId
  { gameId :: Text
  }
  deriving (Eq, Show, Ord)

instance FromHttpApiData GameId where
  parseUrlPiece = Right . GameId
