{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Api (runServer, Move (..), MoveKey (..)) where

import BasicPrelude
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Lens
import Data.Aeson (FromJSON (..), ToJSON, decode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Semigroup
import qualified Data.Text.Lazy as TL
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import Game
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

moveString :: Move -> Text
moveString = tshow

type API =
  "api"
    :> ( ("join" :> "O" :> WebSocket)
           :<|> ("join" :> "X" :> WebSocket)
       )

server :: MVars -> Server API
server (MVars (Players oSeat xSeat) finishedVar) = websocketsAppFromSeat oSeat finishedVar :<|> websocketsAppFromSeat xSeat finishedVar

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
app :: MVars -> Application
app seats = serve (Proxy :: Proxy API) $ server seats

type OPlayer = MVar Connection

type XPlayer = MVar Connection

type FinishedSignal = MVar ()

data MVars = MVars Players FinishedSignal

data Players = Players OPlayer XPlayer

type PlayerMove f = Board -> f Move

moveGrid :: Grid Move
moveGrid = Grid (Row NW N NE) (Row W C E) (Row SW S SE)

prepareSeats :: IO MVars
prepareSeats = MVars <$> (Players <$> newEmptyMVar <*> newEmptyMVar) <*> newEmptyMVar

-- Main Function
runServer :: IO ()
runServer = do
  seats <- prepareSeats
  _ <- forkIO $ hostGame seats
  putStrLn "Running on http://localhost:8080/"
  run 8080 (app seats)

hostGame :: MVars -> IO ()
hostGame (MVars (Players oSeat xSeat) _) = do
  oConn <- takeMVar oSeat
  xConn <- takeMVar xSeat
  _ <- play (sendUpdateToClients oConn xConn) $ getMoveFromPlayers (playerMoveFromConnection oConn) (playerMoveFromConnection xConn)
  forever $ threadDelay 10000

getMoveFromPlayers :: PlayerMove f -> PlayerMove f -> GetMove f
getMoveFromPlayers oMove _ (GameInPlay O board) = oMove board
getMoveFromPlayers _ xMove (GameInPlay X board) = xMove board

emptyWonGrid :: Grid Bool
emptyWonGrid = pure False

sendUpdateToClients :: Connection -> Connection -> Update -> IO ()
sendUpdateToClients oConn xConn (Update board status) = case status of
  Unfinished player -> sendHtml oConn (unfinishedBoardHtml thisPlayer) *> sendHtml xConn (unfinishedBoardHtml otherPlayer)
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
    unfinishedBoardHtml player = boardHtml emptyWonGrid (Active player)

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

play :: (Monad f) => SendMessage f -> GetMove f -> f FinishedGame
play notify getMove = do
  result <- fixPlay (doAndRecurse notifyPlaying . playUnfixed getMove)
  notifyResult result
  return result
  where
    notifyPlaying (GameInPlay player board) = notify . Update board $ Unfinished player
    doAndRecurse f recurse a = f a *> recurse a
    notifyResult (FinishedGame board result) = notify . Update board $ FinishedStatus result

gridForWinningLine :: WinningLine -> Grid Bool
gridForWinningLine = foldr f emptyWonGrid
  where
    f move = set (boardLens move) True

data MoveKey = MoveObject {moveKey :: Move} deriving (Generic)

instance ToJSON MoveKey

instance FromJSON MoveKey
