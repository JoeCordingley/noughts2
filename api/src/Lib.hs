{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lib (runServer, Move (..), MoveKey (..)) where

import BasicPrelude
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Lens
import Data.Aeson (FromJSON (..), ToJSON, decode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Semigroup
import qualified Data.Text.Lazy as TL
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import Lucid (term)
import Lucid.Base (Attributes, Html, renderText)
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Network.WebSockets.Connection (Connection, receiveData, sendTextData, withPingThread)
import Servant
import Servant.API.WebSocket (WebSocket)

wsSend :: Attributes
wsSend = term "ws-send" mempty

hxVals :: Text -> Attributes
hxVals = term "hx-vals"

-- API Definition
data Grid a = Grid {_top :: Row a, _middle :: Row a, _bottom :: Row a}

data Row a = Row {_left :: a, _center :: a, _right :: a}

makeLenses ''Row
makeLenses ''Grid

moveString :: Move -> Text
moveString = tshow

type API =
  "api"
    :> ( ("join" :> "O" :> WebSocket)
           :<|> ("join" :> "X" :> WebSocket)
       )

server :: Seats -> Server API
server (Seats (Players oSeat xSeat) finishedVar) = websocketsAppFromSeat oSeat finishedVar :<|> websocketsAppFromSeat xSeat finishedVar

playerMoveFromConnection :: Connection -> PlayerMove IO
playerMoveFromConnection conn = const $ do
  msg <- receiveData conn
  case decode msg of
    Just (MoveObject move) -> pure move
    Nothing -> fail "Invalid move received"

websocketsAppFromSeat :: MVar Connection -> MVar () -> Server WebSocket
websocketsAppFromSeat seat finishedVar conn = liftIO $ keepAlive conn communication
  where
    communication = do
      putStrLn "Connected!"
      sendTextData conn $ renderText $ p_ [id_ "board"] "Waiting for opponent..."
      putMVar seat conn
      readMVar finishedVar

keepAlive :: Connection -> IO c -> IO c
keepAlive conn =
  withPingThread conn 30 (pure ())

-- Application
app :: Seats -> Application
app seats = serve (Proxy :: Proxy API) $ server seats

data Seats = Seats {players :: Players (MVar Connection), finished :: MVar ()}

data Players a = Players {o :: a, x :: a}

type PlayerMove f = Board -> f Move

data Move = NW | N | NE | W | C | E | SW | S | SE deriving (Show, Eq, Generic)

instance FromJSON Move

instance ToJSON Move

moves :: [Move]
moves = [NW, N, NE, W, C, E, SW, S, SE]

moveGrid :: Grid Move
moveGrid = Grid (Row NW N NE) (Row W C E) (Row SW S SE)

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
hostGame (Seats (Players oSeat xSeat) finishedVar) = do
  oConn <- takeMVar oSeat
  xConn <- takeMVar xSeat
  _ <- play (sendUpdateToClients oConn xConn) $ getMoveFromPlayers (playerMoveFromConnection oConn) (playerMoveFromConnection xConn)
  forever $ threadDelay 10000

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

getMoveFromPlayers :: PlayerMove f -> PlayerMove f -> GetMove f
getMoveFromPlayers oMove _ (GameInPlay O board) = oMove board
getMoveFromPlayers _ xMove (GameInPlay X board) = xMove board

emptyWonGrid :: Grid Bool
emptyWonGrid = pure False

sendUpdateToClients :: Connection -> Connection -> Update -> IO ()
sendUpdateToClients oConn xConn (Update board status) = case status of
  Unfinished player -> sendHtml oConn (boardHtml emptyWonGrid (Active thisPlayer)) *> sendHtml xConn (boardHtml emptyWonGrid (Active otherPlayer))
    where
      (thisPlayer, otherPlayer) = case player of
        O -> (ThisPlayer, OtherPlayer)
        X -> (OtherPlayer, ThisPlayer)
  FinishedStatus result -> sendHtml oConn (finishedBoardHtml O) *> sendHtml xConn (finishedBoardHtml X)
    where
      finishedBoardHtml player = boardHtml winningSquares (Ended clientResult)
        where
          (clientResult, winningSquares) = case result of
            WonGame winner winningLines' ->
              if player == winner
                then (Won ThisPlayer, winningSquares')
                else (Won OtherPlayer, winningSquares')
              where
                winningSquares' = fmap getAny $ foldMap (fmap Any . gridForWinningLine) winningLines'
            DrawnGame -> (Drawn, emptyWonGrid)
  where
    boardHtml :: Grid Bool -> Activity -> Html ()
    boardHtml wonGrid activity = div_ [class_ "board", id_ "board"] $ sequence_ (square activity <$> wonGrid <*> moveGrid <*> board) *> div_ [class_ "status"] statusMessage
      where
        statusMessage = case activity of
          Active ThisPlayer -> "your turn"
          Active OtherPlayer -> "their turn"
          Ended Drawn -> "draw"
          Ended (Won ThisPlayer) -> "you won!"
          Ended (Won OtherPlayer) -> "they won :("

sendHtml :: Connection -> Html () -> IO ()
sendHtml conn html = sendTextData conn $ renderText html

data Update = Update Board GameStatus

data Result = WonGame Player [WinningLine] | DrawnGame

data GameStatus = Unfinished Player | FinishedStatus Result

data ClientResult = Drawn | Won ThisOrOtherPlayer

type WonSquare = Bool

square :: Activity -> WonSquare -> Move -> Space -> Html ()
square activity wonSquare move space = button_ attrs content
  where
    attrs =
      defaultAttrs <> case (activity, space) of
        (Active ThisPlayer, Nothing) -> activeAttrs
        _ -> if wonSquare then [class_ "winning-cell"] else []
    defaultAttrs = [id_ $ moveString move, class_ "cell"]
    activeAttrs = [wsSend, hxVals $ encodeToText $ MoveObject move]
    content = case space of
      Nothing -> " "
      Just player -> case player of
        O -> "O"
        X -> "X"

data ThisOrOtherPlayer = ThisPlayer | OtherPlayer

data Activity = Active ThisOrOtherPlayer | Ended ClientResult

encodeToText :: (ToJSON a) => a -> Text
encodeToText = TL.toStrict . encodeToLazyText

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
  Just wonLines -> FinishedStatus $ WonGame player wonLines
  Nothing -> if all marked board then FinishedStatus DrawnGame else Unfinished (switch player)
  where
    wonGame = foldMap (fmap singleton . winningLine) winningLines
    winningLine spaces =
      if all markedByThisPlayer spaces
        then Just spaces
        else Nothing
      where
        markedByThisPlayer space = view (boardLens space) board == Just player

switch :: Player -> Player
switch O = X
switch X = O

winningLines :: [WinningLine]
winningLines =
  [ [NW, N, NE],
    [W, C, E],
    [SW, S, SE],
    [NW, W, SW],
    [N, C, S],
    [NE, E, SE],
    [NW, C, SE],
    [NE, C, SW]
  ]

gridForWinningLine :: WinningLine -> Grid Bool
gridForWinningLine = foldr f emptyWonGrid
  where
    f move = set (boardLens move) True

type Space = Maybe Player

marked :: Space -> Bool
marked = isJust

data Game = Playing GameInPlay | Finished Result

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

instance (Semigroup a) => Semigroup (Grid a) where
  l <> r = (<>) <$> l <*> r

instance (Monoid a) => Monoid (Grid a) where
  mempty = pure mempty

type WinningLine = [Move]

data Player = O | X deriving (Eq)

data MoveKey = MoveObject {moveKey :: Move} deriving (Generic)

instance ToJSON MoveKey

instance FromJSON MoveKey
