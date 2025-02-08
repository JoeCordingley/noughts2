{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lib (runServer) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad (forever, void, (<=<), (>=>))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)

-- import Lucid (Attributes, term)
import Lucid.Base (Html)
import Lucid.Html5
import Network.Wai.Handler.Warp (run)
import Network.WebSockets.Connection (Connection, receiveData, withPingThread)
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

-- wsSend :: Attributes
-- wsSend = term "ws-send" mempty

-- hxVals :: Text -> Attributes
-- hxVals = term "hx-vals"

-- API Definition
type API =
    "api"
        :> ( "message" :> Get '[HTML] (Html ())
                :<|> ("join" :> "O" :> WebSocket)
                :<|> ("join" :> "X" :> WebSocket)
           )

server :: Seats -> Server API
server (Seats oSeat xSeat finished) = pure messageContent :<|> websocketsApp oSeat finished :<|> websocketsApp xSeat finished

playerFromConnection :: Connection -> PlayerInteractions
playerFromConnection conn = undefined

websocketsApp :: MVar PlayerInteractions -> MVar () -> Server WebSocket
websocketsApp seat finished conn = keepAlive conn communication
  where
    communication :: ExceptT ServerError IO ()
    communication = liftIO $ do
        putMVar seat (playerFromConnection conn)
        readMVar finished

keepAlive :: (MonadError e m, MonadIO m) => Connection -> ExceptT e IO c -> m c
keepAlive conn =
    liftEither <=< liftIO . withPingThread conn 30 (pure ()) . runExceptT

-- HTMX Response Content
messageContent :: Html ()
messageContent = p_ "Hello from the server! This was loaded via HTMX."

-- Application
app :: Seats -> Application
app seats = do
    serve (Proxy :: Proxy API) $ server seats

data Seats = Seats {o :: MVar PlayerInteractions, x :: MVar PlayerInteractions, finished :: MVar ()}

data PlayerInteractions = PlayerInteractions

data Move = NW | N | NE | W | C | E | SW | S | SE

data Update = Update Move PostTurnStatus

prepareSeats :: IO Seats
prepareSeats = f <$> newEmptyMVar <*> newEmptyMVar <*> newEmptyMVar
  where
    f o x f = Seats o x f

-- Main Function
runServer :: IO ()
runServer = do
    seats <- prepareSeats
    _ <- forkIO $ hostGame seats
    putStrLn "Running on http://localhost:8080/"
    run 8080 (app seats)

hostGame :: Seats -> IO ()
hostGame (Seats oSeat xSeat finished) = do
    oPlayer <- takeMVar oSeat
    xPlayer <- takeMVar xSeat
    play sendUpdate $ getMove oPlayer xPlayer
    putMVar finished ()

type GetMove f = Game -> f Move

applyMove :: Game -> Move -> PostTurnStatus
applyMove (Game player board) move = case wonGame of
    Just wonLines -> Finished $ WonGame wonLines
    Nothing -> Unfinished $ Game (switch player) newBoard
  where
    newBoard = undefined
    wonGame = undefined
    switch O = X
    switch X = O

getMove :: PlayerInteractions -> PlayerInteractions -> GetMove IO
getMove = undefined

sendUpdate :: Update -> IO ()
sendUpdate = undefined

type SendUpdate f = Update -> f ()

play :: Monad f => SendUpdate f -> GetMove f -> f Result
play notify getMove = play' startingGame
  where
    play' game = do
        status <- playTurn game
        case status of
            Unfinished newGame -> play' newGame
            Finished result -> pure result
      where
        playTurn = updateStatus <=< getMove
        updateStatus move = status <$ (notify $ Update move status)
          where
            status = applyMove game move
    startingGame = Game O startingBoard
    startingBoard = pure Nothing

-- playGame :: GetMove -> UpdateStatus -> IO ()
-- playGame = play O startingBoard where
--   play player board = do
--     move <- getMove player
--     case applyMove player move board of
--       Unfinished newBoard -> do
--         updateStatus $ PlayingStatus move (switch player)
--         play (switch player) newBoard
--       Finished -> updateStatus $ FinishedStatus

data PostTurnStatus = Unfinished Game | Finished Result

data Game = Game {currentPlayer :: Player, board :: Board}
type Board = Grid (Maybe Player)

data Grid a = Grid {top :: Row a, middle :: Row a, bottom :: Row a}
data Row a = Row {left :: a, center :: a, right :: a}
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

data Result = WonGame [WinningLine] | DrawnGame
type UpdateStatus = Update -> IO ()

data WinningLine = TopRow | MiddleRow | BottomRow | LeftColumn | CenterColumn | RightColumn | DiagonalNWSE | DiagonalNESW

data Player = O | X
