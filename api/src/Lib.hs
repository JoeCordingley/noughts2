{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lib (runServer) where

import BasicPrelude
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Lens
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import GHC.Conc (threadDelay)
import Lucid (term)
import Lucid.Base (Attributes, Html, renderText)
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Network.WebSockets.Connection (Connection, receiveData, sendTextData, withPingThread)
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.WebSocket (WebSocket)

-- Custom HTMX Attributes
-- hxGet :: Text -> Attributes
-- hxGet = term "hx-get"
--
-- hxTarget :: Text -> Attributes
-- hxTarget = term "hx-target"
--
-- hxSwap :: Text -> Attributes
-- hxSwap = term "hx-swap"
--
-- hxExt :: Text -> Attributes
-- hxExt = term "hx-ext"
--
-- wsConnect :: Text -> Attributes
-- wsConnect = term "ws-connect"

wsSend :: Attributes
wsSend = term "ws-send" mempty

-- hxVals :: Text -> Attributes
-- hxVals = term "hx-vals"

-- API Definition
data Grid a = Grid {_top :: Row a, _middle :: Row a, _bottom :: Row a}

data Row a = Row {_left :: a, _center :: a, _right :: a}

makeLenses ''Row
makeLenses ''Grid

moveString :: Move -> Text
moveString = tshow

type API =
  "api"
    :> ( "message" :> Get '[HTML] (Html ())
           :<|> ("join" :> "O" :> WebSocket)
           :<|> ("join" :> "X" :> WebSocket)
       )

server :: Seats -> Server API
server (Seats (Players oSeat xSeat) finished) = pure messageContent :<|> websocketsApp oSeat finished :<|> websocketsApp xSeat finished

-- data PlayerInteractions = PlayerInteractions {playerMove :: PlayerMove IO, playerNotify :: PlayerUpdate -> IO ()}
playerMove :: Connection -> PlayerMove IO
playerMove conn _ = do
  json <- receiveData conn
  putStrLn $ "Received: " <> json
  undefined

data PlayerUpdate

-- playerFromConnection :: Connection -> PlayerInteractions
-- playerFromConnection conn = undefined

websocketsApp :: MVar Connection -> MVar () -> Server WebSocket
websocketsApp seat finished conn = keepAlive conn communication
  where
    communication :: ExceptT ServerError IO ()
    communication = liftIO $ do
      putStrLn "Connected!"
      sendTextData conn $ renderText $ p_ [id_ "board"] "Waiting for opponent..."
      putMVar seat conn
      readMVar finished

keepAlive :: (MonadError e m, MonadIO m) => Connection -> ExceptT e IO c -> m c
keepAlive conn =
  liftEither <=< liftIO . withPingThread conn 30 (pure ()) . runExceptT

-- HTMX Response Content
messageContent :: Html ()
messageContent = p_ "Hello from the server! This was loaded via HTMX."

-- Application
app :: Seats -> Application
app seats = do serve (Proxy :: Proxy API) $ server seats

data Seats = Seats {players :: Players (MVar Connection), finished :: MVar ()}

data Players a = Players {o :: a, x :: a}

type PlayerMove f = Board -> f Move

data Move = NW | N | NE | W | C | E | SW | S | SE deriving (Show, Eq)

moves :: [Move]
moves = [NW, N, NE, W, C, E, SW, S, SE]

data Update = Update Board GameStatus

prepareSeats :: IO Seats
prepareSeats = Seats <$> (Players <$> newEmptyMVar <*> newEmptyMVar) <*> newEmptyMVar

-- Main Function
runServer :: IO ()
runServer = do
  seats <- prepareSeats
  _ <- forkIO $ hostGame seats
  putStrLn "Running on http://localhost:8080/"
  run 8080 (app seats)

hostGame :: Seats -> IO ()
hostGame (Seats (Players oSeat xSeat) finished) = do
  oPlayer <- takeMVar oSeat
  xPlayer <- takeMVar xSeat
  _ <- play (sendUpdate oPlayer xPlayer) $ getMove (playerMove oPlayer) (playerMove xPlayer)
  putMVar finished ()

type GetMove f = GameInPlay -> f Move

type BoardLens = forall a. Lens' (Grid a) a

boardLens :: Move -> BoardLens
boardLens NW = top . left
boardLens N = top . center
boardLens NE = top . right
boardLens W = middle . left
boardLens C = middle . center
boardLens E = middle . right
boardLens SW = bottom . left
boardLens S = bottom . center
boardLens SE = bottom . right

getMove :: PlayerMove f -> PlayerMove f -> GetMove f
getMove oPlayer _ (GameInPlay O board) = oPlayer board
getMove _ xPlayer (GameInPlay X board) = xPlayer board

sendUpdate :: Connection -> Connection -> Update -> IO ()
sendUpdate oPlayer xPlayer (Update board status) = case status of
  Unfinished player -> sendTextData activePlayer activeBoard *> sendTextData inactivePlayer inactiveBoard
    where
      (activePlayer, inactivePlayer) = case player of
        O -> (oPlayer, xPlayer)
        X -> (xPlayer, oPlayer)
      activeBoard = boardHtml
      inactiveBoard = boardHtml
      boardHtml = renderText $ div_ [class_ "board", id_ "board"] $ mapM_ square moves
        where
          square :: Move -> Html ()
          square move = button_ [id_ $ moveString move, class_ "cell", wsSend] $ ""

type SendMessage f = Update -> f ()

startingPlayer :: Player
startingPlayer = O

firstUpdate :: Update
firstUpdate = Update startingBoard (Unfinished startingPlayer)

play :: (Monad f) => SendMessage f -> GetMove f -> f Result
play notify getMove = notify firstUpdate *> play' startingGame
  where
    play' gameInPlay = do
      status <- playTurn
      case status of
        Playing newGame -> play' newGame
        Finished result -> pure result
      where
        playTurn = getMove gameInPlay >>= updateStatus
        updateStatus move = game <$ (notify $ Update newBoard status)
          where
            newBoard = set (boardLens move) (Just player) board
            status = postTurnStatus player newBoard
            game = case status of
              Unfinished nextPlayer -> Playing $ GameInPlay nextPlayer newBoard
              FinishedStatus result -> Finished result
        GameInPlay player board = gameInPlay

startingGame :: GameInPlay
startingGame = GameInPlay O startingBoard

startingBoard :: Board
startingBoard = pure Nothing

postTurnStatus :: Player -> Board -> GameStatus
postTurnStatus player board = case wonGame of
  Just wonLines -> FinishedStatus $ WonGame wonLines
  Nothing -> if all marked board then FinishedStatus DrawnGame else Unfinished (switch player)
  where
    wonGame = foldMap (fmap singleton . uncurry winningLine) winningLines
    winningLine line spaces =
      if all markedByThisPlayer spaces
        then Just line
        else Nothing
    markedByThisPlayer space = view (boardLens space) board == Just player

switch :: Player -> Player
switch O = X
switch X = O

winningLines :: [(WinningLine, [Move])]
winningLines =
  [ (TopRow, [NW, N, NE]),
    (MiddleRow, [W, C, E]),
    (BottomRow, [SW, S, SE]),
    (LeftColumn, [NW, W, SW]),
    (CenterColumn, [N, C, S]),
    (RightColumn, [NE, E, SE]),
    (DiagonalNWSE, [NW, C, SE]),
    (DiagonalNESW, [NE, C, SW])
  ]

type Space = Maybe Player

marked :: Space -> Bool
marked = isJust

data Game = Playing GameInPlay | Finished Result

data GameStatus = Unfinished Player | FinishedStatus Result

data GameInPlay = GameInPlay Player Board

type Board = Grid Space

instance Functor Row where
  fmap f (Row l c r) = Row (f l) (f c) (f r)

instance Applicative Row where
  pure a = Row a a a
  Row fl fc fr <*> Row l c r = Row (fl l) (fc c) (fr r)

instance Functor Grid where
  fmap f (Grid t m b) = Grid (fmap f t) (fmap f m) (fmap f b)

instance Applicative Grid where
  pure a = Grid (pure a) (pure a) (pure a)
  Grid ft fm fb <*> Grid t m b = Grid (ft <*> t) (fm <*> m) (fb <*> b)

instance Foldable Row where
  foldMap f (Row l c r) = f l <> f c <> f r

instance Foldable Grid where
  foldMap f (Grid t m b) = foldMap f t <> foldMap f m <> foldMap f b

data Result = WonGame [WinningLine] | DrawnGame

data WinningLine = TopRow | MiddleRow | BottomRow | LeftColumn | CenterColumn | RightColumn | DiagonalNWSE | DiagonalNESW

data Player = O | X deriving (Eq)

data HxTriggered a = HxTriggered a

data MoveRequest = MoveRequest Move

instance (FromJSON a) => FromJSON (HxTriggered a) where
  parseJSON = withObject "HxTriggered" $ \o -> do
    headers <- o .: "HEADERS"
    hxTrigger <- headers .: "HX-Trigger"
    a <- parseJSON hxTrigger
    return $ HxTriggered a

-- t :: Lens' (Grid a) (Row a)
-- t f s = fmap g $ f $ top s
--  where
--    g a = s {top = a}
--
-- m :: Lens' (Grid a) (Row a)
-- m f s = fmap g $ f $ middle s
--  where
--    g a = s {middle = a}
--
-- b :: Lens' (Grid a) (Row a)
-- b f s = fmap g $ f $ bottom s
--  where
--    g a = s {bottom = a}
--
