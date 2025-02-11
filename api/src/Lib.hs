{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lib (runServer) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Lens
import Control.Monad (forever, void, (<=<), (>=>))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Data.Text (Text)

-- import Lucid (Attributes, term)

import Data.List (singleton)
import Data.Maybe (isJust)
import qualified Data.Text.IO as Text (putStrLn)
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
data Grid a = Grid {_top :: Row a, _middle :: Row a, _bottom :: Row a}

data Row a = Row {_left :: a, _center :: a, _right :: a}

makeLenses ''Row
makeLenses ''Grid

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

getMove :: PlayerInteractions -> PlayerInteractions -> GetMove IO
getMove = undefined

sendUpdate :: Update -> IO ()
sendUpdate = undefined

type SendUpdate f = Update -> f ()

play :: (Monad f) => SendUpdate f -> GetMove f -> f Result
play notify getMove = play' startingGame
  where
    play' game = do
      status <- playTurn
      case status of
        Unfinished newGame -> play' newGame
        Finished result -> pure result
      where
        playTurn = getMove game >>= updateStatus
        updateStatus move = status <$ (notify $ Update move status)
          where
            status = postTurnStatus player $ applyMove board
            applyMove = set (boardLens move) (Just player)
        Game player board = game

startingGame :: Game
startingGame = Game O startingBoard

startingBoard :: Board
startingBoard = pure Nothing

postTurnStatus :: Player -> Board -> PostTurnStatus
postTurnStatus player board = case wonGame of
  Just wonLines -> Finished $ WonGame wonLines
  Nothing -> if all marked board then Finished DrawnGame else Unfinished $ Game (switch player) board
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

data PostTurnStatus = Unfinished Game | Finished Result

data Game = Game Player Board

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
