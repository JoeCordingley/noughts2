{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Api () where

import BasicPrelude
import Control.Concurrent.STM
import GHC.Generics (Generic)
import Lib
import Lucid.Base (Html)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets.Connection (Connection, receiveData)
import Servant
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.WebSocket (WebSocket)
import Tigris.Api (Dynasty)

type Api = "api" :> ("tigris" :> GameApi :<|> "noughts" :> GameApi)

type GameResponse = Html ()

type GameApi =
  "create" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[JSON] CreateGameResponse
    :<|> Capture "gameId" GameId
      :> ( Header "Cookie" Text :> Get '[HTML] GameResponse
             :<|> "join" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[HTML] JoinGameResponse
             :<|> "play" :> Header "Cookie" Text :> WebSocket
         )

type CreateGameResponse = Headers '[Header "HX-Redirect" Text, Header "Set-Cookie" Text] NoContent

type JoinGameResponse = Headers '[Header "Set-Cookie" Text] (Html ())

runServer :: IO ()
runServer = do
  gameMaps <- newGameMaps
  putStrLn "Running on http://localhost:8080/"
  run 8080 (serve (Proxy :: Proxy Api) (server gameMaps))

newGameMaps :: IO GameMaps
newGameMaps = undefined

data GameMaps = GameMaps
  { getNoughtsMap :: TVar (GameMap NoughtOrCross),
    getTigrisMap :: TVar (GameMap Dynasty)
  }

data NoughtOrCross = Nought | Cross deriving (Eq, Ord, Show, Generic)

type GameMap a = Map GameId (Game a)

data Game a = Game
  { latestState :: TVar (Map a (PlayerId, Name)),
    playerOutputs :: TVar [Map a (PlayerId, Name) -> STM ()],
    playerInputs :: TVar [(PlayerId, STM a)],
    playerNames :: TVar (Map PlayerId Name),
    waitForFinish :: IO ()
  }

server :: GameMaps -> Server Api
server = undefined

--
-- server :: TVar (GameMap a) -> Server GameApi
-- server games = createGame games :<|> gameEndpoints
--  where
--    gameEndpoints id = gameHandler games id :<|> joinGame games id :<|> playGameHandler games id
--
-- generateGameId :: IO GameId
-- generateGameId = GameId <$> generateId
--
-- createNewGame :: TVar (GameMap a) -> PlayerId -> Name -> IO GameId
-- createNewGame games host name = do
--  generatedId <- generateGameId
--  game <- atomically $ do
--    game <- newGame
--    modifyTVar games $ Map.insert generatedId game
--    addPlayer host name game
--    return game
--  forkIO $ hostGame game
--  pure generatedId
--
-- withGame gamesVar gameId action = do
--  gameMap <- liftIO $ readTVarIO gamesVar
--  case Map.lookup gameId gameMap of
--    Just game -> action game
--    Nothing -> throwError err404
--
-- gameHandler :: String -> TVar (GameMap a) -> GameId -> Maybe Text -> Handler GameResponse
-- gameHandler tigris games id maybeCookies = withGame games id f
--  where
--    f _ = return $ case maybeCookies >>= playerIdCookie of
--      Just _ -> websocketHtml id
--      Nothing ->
--        form_ [term "hx-post" ("/api/" <> tigris <> "/" <> gameId id <> "/join")] $ do
--          input_ [id_ "name", name_ "name", type_ "text"]
--          button_ [type_ "submit"] "Join"
--
-- createGame :: String -> TVar (GameMap a) -> [(Text, Text)] -> Handler CreateGameResponse
-- createGame tigris games formData = case lookup "name" formData of
--  Just name -> liftIO $ do
--    putStrLn $ "Creating game for player: " <> name
--    playerId <- newPlayerId
--    GameId newId <- createNewGame games playerId name
--    return $ addHeader ("/games/" <> tigris <> "/" <> uewId) $ addPlayerIdCookie playerId NoContent
--  Nothing -> throwError err400
--
-- seatPlayers :: Game a -> Map a PlayerId -> IO (Map a PlayerId)
-- seatPlayers game = firstNotification >=> setupGameSTM receiveMsg notify
--  where
--    firstNotification = atomically . returning notify
--    notify = withNames >=> notifyPlayers
--    withNames m = fmap (`composeMapWithInput` m) . readTVar $ playerNames game
--    receiveMsg = (readTVar $ playerInputs game) >>= nextPlayerMessage
--    nextPlayerMessage = foldr orElse retry . map (uncurry $ fmap . TakePosition)
--    notifyPlayers playerMap = do
--      readTVar (playerOutputs game) >>= traverse_ ($ playerMap)
--      writeTVar (latestState game) playerMap
--
-- hostGame :: Game a -> IO ()
-- hostGame game = do
--  playerMap <- seatPlayers game startingState
--  playGame playerMap
--  where
--    startingState = Map.empty
--
-- addConnection :: (Map a (PlayerId, Name) -> PlayerId -> Html ()) -> Connection -> Game a -> PlayerId -> IO ()
-- addConnection chooseCharacter conn game player = do
--  -- Create a personal notification queue
--  outputQueue <- newTQueueIO
--  inputQueue <- newTQueueIO
--  -- Atomically register the player and queue
--  state <- atomically $ do
--    modifyTVar' (playerOutputs game) (writeTQueue outputQueue :)
--    modifyTVar' (playerInputs game) ((player, readTQueue inputQueue) :)
--    state <- readTVar (latestState game)
--    writeTQueue outputQueue state
--    return state
--
--  -- Start sender and receiver threads
--  withAsync (sendLoop chooseCharacter outputQueue conn player) $ \sender ->
--    withAsync (readLoop conn inputQueue) $ \reader ->
--      waitForFinish game *> cancel sender *> cancel reader
--
-- newGame :: STM (Game a)
-- newGame = do
--  notify <- newTVar []
--  ps <- newTVar []
--  latestState <- newTVar Map.empty
--  names <- newTVar Map.empty
--  return Game {latestState = latestState, playerOutputs = notify, playerInputs = ps, waitForFinish = forever $ threadDelay 10000, playerNames = names}
--
-- addPlayer :: PlayerId -> Name -> Game a -> STM ()
-- addPlayer playerId name =
--  flip modifyTVar' (Map.insert playerId name)
--    . playerNames
--
-- playGameHandler games gameId maybeCookies conn = withGame games gameId $ connectGame maybeCookies conn
--
-- connectGame chooseCharacter maybeCookies conn game = do
--  putStrLn "connected"
--  player <- case maybeCookies >>= playerIdCookie of
--    Just id -> return id
--    _ -> do
--      liftIO $ putStrLn $ "error: " <> tshow maybeCookies
--      throwError err400
--  liftIO $ keepAlive conn $ addConnection chooseCharacter conn game player
--
--
-- data GameId = GameId {gameId :: Text} deriving (Eq, Show, Ord)
--
-- setupGame :: (Monad m) => m (SetupMessage a) -> (Map a PlayerId -> m b) -> (Map a PlayerId -> m b) -> Map a PlayerId -> m b
-- setupGame receive end recurse playerMap = do
--  message <- receive
--  case message of
--    TakePosition player position -> recurse $ takePosition playerMap
--      where
--        takePosition = if Map.notMember position playerMap then Map.insert position player . Map.filter (/= player) else id
--    StartGame -> end playerMap
--
-- data SetupMessage a = TakePosition PlayerId a | StartGame deriving (Generic, Show)
--
-- data Role = Host | Guest deriving (Eq, Ord, Show)
--
-- data Player = Player Role PlayerId Name deriving (Eq, Ord, Show)
--
-- instance FromHttpApiData GameId where
--  parseUrlPiece = Right . GameId
--
-- type PlayerMap = Map Dynasty PlayerId
--
-- type NotifyPlayer = PlayerMap -> IO ()
--
---- type NoughtsGame = Game Dynasty
--
---- type NoughtsMap = GameMap Dynasty
--
---- Servant API
--
-- addPlayerIdCookie :: PlayerId -> NoContent -> Headers '[Header "Set-Cookie" Text] NoContent
-- addPlayerIdCookie (PlayerId playerId) content = addHeader (cookie playerIdKey playerId) content
--
-- cookie :: Text -> Text -> Text
-- cookie key value = key <> "=" <> value
--
---- Server Setup
--
-- websocketHtml :: GameId -> Html ()
-- websocketHtml id = div_ [id_ "game", term "hx-ext" "ws", term "ws-connect" ("/api/tigris/" <> gameId id <> "/play")] $ div_ [id_ "board"] $ return ()
--
---- Game Creation
--
---- Send updates from the queue to the connection
-- sendLoop chooseCharacter queue conn player = forever $ do
--  state <- atomically $ readTQueue queue
--  sendHtml conn $ chooseCharacter state player
--
---- Dummy read loop (simulate receiving inputs)
-- readLoop :: Connection -> TQueue a -> IO ()
-- readLoop conn queue = forever $ do
--  msg <- receiveData conn
--  case decode msg of
--    Just (CharacterChoice character) -> do
--      putStrLn $ "message: " <> tshow character
--      atomically $ writeTQueue queue $ character
--    Nothing -> do
--      return () -- Handle decoding failure
--
---- Game Logic Core
--
-- composeMapWithInput :: (Ord a) => Map a b -> Map k a -> Map k (a, b)
-- composeMapWithInput = Map.mapMaybe . withInput . flip Map.lookup
--  where
--    withInput f a = (a,) <$> f a
--
-- notifySetup :: STM [Map Dynasty (PlayerId, Name) -> STM ()] -> Map Dynasty (PlayerId, Name) -> STM ()
-- notifySetup playersVar playerMap = playersVar >>= traverse_ ($ playerMap)
--
-- setupGameSTM :: STM (SetupMessage a) -> (Map a PlayerId -> STM ()) -> Map a PlayerId -> IO (Map a PlayerId)
-- setupGameSTM receive notify = fixAtomically $ setupGame receive (pure . pure) . notifying
--  where
--    notifying recurse = returning notify >=> recurse
--
-- fixAtomically :: ((a -> STM (IO b)) -> a -> STM (IO b)) -> a -> IO b
-- fixAtomically f = fix $ \recurse -> join . atomically . (f $ pure . recurse)
--
-- playGame :: a -> IO ()
-- playGame = undefined
--
-- playerIdKey :: Text
-- playerIdKey = "playerId"
--
-- playerIdCookie :: Text -> Maybe PlayerId
-- playerIdCookie = fmap PlayerId . getCookie playerIdKey
--
-- getCookie :: Text -> Text -> Maybe Text
-- getCookie key =
--  lookup key . parseCookiesText . TE.encodeUtf8
--
-- generateId :: IO Text
-- generateId = stringRandomIO "[a-zA-Z0-9]{5}"
--
-- newPlayerId :: IO PlayerId
-- newPlayerId = PlayerId <$> generateId
