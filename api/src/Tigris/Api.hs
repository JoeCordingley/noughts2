{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tigris.Api (Dynasty, gameServer) where

import BasicPrelude 
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVarIO, readTVarIO, atomically)
import Foreign.Store (lookupStore, readStore, newStore)
import Control.Monad.Random.Lazy
import qualified Data.Map as Map
import Data.Map ((!))
import GHC.Generics (Generic)
import Lib
import Lucid (term, toHtml, Attributes)
import Lucid.Base (Html)
import Lucid.Html5 hiding (for_) 
import Tigris.Game
import Tigris.Data
import Data.Aeson (FromJSON, ToJSON)
import Servant (Server)
import Api (GameApi, actionsApi, responses, paths)
import Control.Lens
import qualified Data.Array as Array
import GHC.Conc (threadDelay)

gameServer :: IO (Server GameApi)
gameServer = actionsApi (responses $ paths Tigris) <$> tigrisActions

tigrisActions :: IO Actions
tigrisActions = do
  mStore <- lookupStore 0
  tvar <- case mStore of
    Just store -> readStore store
    Nothing -> do
      t <- newTVarIO Map.empty
      _ <- newStore t
      return t
  games <- readTVarIO tvar
  traverse_ (forkIO . hostGame) games
  return $ actions (newGame openingState) hostGame (table $ flip gameHtml) tvar
  where
    hostGame game = do
      st <- readTVarIO (latestState game)
      case st of
        SeatingPlayers m -> do
          dynastyMap <- seatPlayers game m
          hostGameAfterSeating game dynastyMap
        Playing dynasties playingState -> play game dynasties playingState
    hostGameAfterSeating game dynastyMap = play game dynastyMap =<< evalRandIO (setupGame dynastyMap)

openingState :: GameState
openingState = SeatingPlayers Map.empty

seatPlayers :: GameTVars GameState PlayerSetupMessage -> DynastyMap -> IO DynastyMap
seatPlayers game = fix (recursing (atomically . notify game . SeatingPlayers) . seatPlayersUnfixed (fmap (uncurry setupMessage) . atomically . nextMessageFromAnyPlayer $ playerInputs game))

seatPlayersUnfixed :: (Monad m) => m SetupMessage -> (DynastyMap -> m DynastyMap) -> DynastyMap -> m DynastyMap
seatPlayersUnfixed receive recurse playerMap = do
  message <- receive
  case message of
    TakePosition player position -> recurse $ takePosition playerMap
      where
        takePosition = if Map.notMember position playerMap then Map.insert position player . Map.filter (/= player) else id
    Start -> (if atLeastTwo playerMap then pure else recurse) playerMap

atLeastTwo :: Map Dynasty a -> Bool
atLeastTwo playerMap = Map.size playerMap >= 2

data GameResult

play :: GameTVars GameState a -> DynastyMap -> PlayingState -> IO ()
play game dynasties = void . fix (recursing (atomically . notify game . Playing dynasties) . playGame interactions)

data GameState = SeatingPlayers DynastyMap | Playing DynastyMap PlayingState deriving (Show)

gameHtml :: GameState -> Player -> Html ()
gameHtml (SeatingPlayers m) = chooseDynasty m
gameHtml (Playing m playingState) = playingHtml . htmlModel m playingState

xData :: Text -> Attributes
xData = term "x-data" 

data HtmlModel = HtmlModel {htmlGrid :: Grid, isCurrentPlayer :: Bool, leadersInHand :: [Sphere] }

htmlModel :: DynastyMap -> PlayingState -> Player -> HtmlModel
htmlModel m (PlayingState _ dynasties game) player = case dynasties of
  (currentPlayer:_) -> HtmlModel { htmlGrid = (view (board . grid) game), isCurrentPlayer = (m ! currentPlayer) == player, leadersInHand = [Temples]}
  _ -> undefined


xIfTemplate :: Text -> Html () -> Html ()
xIfTemplate predicate = template_ [term "x-if" predicate] . div_

wsSend :: Attributes
wsSend = term "ws-send" mempty

hxVals :: Text -> Attributes
hxVals = term "hx-vals"

playingHtml :: HtmlModel -> Html ()
playingHtml (HtmlModel {htmlGrid, isCurrentPlayer, leadersInHand}) = gameDiv $ div_ [xData "{ action: null }"] $ do
  bool "not your turn" "your turn" isCurrentPlayer
  xIfTemplate "action == 'leader'" $ div_ $ span_ [term "x-text" "sphere"] mempty <> " leader selected"
  boardHtml $ htmlGrid
  when isCurrentPlayer $ traverse_ leaderButton leadersInHand
  where
    leaderButton :: Sphere -> Html ()
    leaderButton sphere = button_ [term "@click" $ "action = 'leader'; sphere = '" <> sphereText' <> "'" ] $ toHtml sphereText' where
      sphereText' = sphereText sphere
    boardHtml g = do
      xIfTemplate "!action" $ boardDiv $ traverse_ inactiveSquare g
      xIfTemplate "action == 'leader'" $ do
        xIfTemplate "sphere == 'temples'" $ boardDiv $ traverse_ (\(i, s) -> leaderSquare (encodeToText $ PositionLeader Temples (Just i)) s) $ Array.assocs g
    boardDiv = div_ [id_ "board", term "x-init" "htmx.process"] 
    piece' = traverse_ pieceHtml 
    inactiveSquare s = div_ ([classes_ $ ["tigris-square", markingText $ view marking s]] ) . piece' $ view slot s
    leaderSquare hxVals' s = div_ ([classes_ $ ["tigris-square", markingText $ view marking s, "clickable"], wsSend, hxVals hxVals'] ) . piece' $ view slot s
    pieceHtml :: Piece -> Html ()
    pieceHtml (TilePiece sphere) = div_ [classes_ ["piece", sphereText sphere]] $ mempty 
    pieceHtml (LeaderPiece _) = undefined

interactions :: Dynasty -> Interactions IO 
interactions _ = Interactions {getCommittedTemples = forever $ threadDelay 10000, getAction= forever $ threadDelay 10000}

classes_ :: [Text] -> Attributes
classes_ = class_ . intercalate " "

chooseDynasty :: DynastyMap -> Player -> Html ()
chooseDynasty playerMap thisPlayer =
  gameDiv $ do
    h2_ "Choose Your Dynasty"
    div_ [class_ "dynasty-grid"]
      $ forM_ [Archer, Bull, Pot, Lion]
      $ dynastyDiv
    when (atLeastTwo playerMap) $ button_ [class_ "start-game action", hxVals startGame, term "ws-send" mempty] "Start Game"
  where
    startGame = encodeToText StartGame
    dynastyDiv :: Dynasty -> Html ()
    dynastyDiv dynasty = div_ ([class_ "dynasty-box"] ++ if isMine then [class_ "mine"] else []) $ do
      strong_ . toHtml $ show dynasty
      small_ $ toHtml status
      span_ [class_ "button-area"] $ unless isTaken $ button_ [class_ "dynasty action", hxVals chooseDynastyJson, wsSend] "Choose"
      where
        (isTaken, isMine, status) = case Map.lookup dynasty playerMap of
          Just player -> (True, player == thisPlayer, "Player: " <> playerName player)
          Nothing -> (False, False, "Available")
        chooseDynastyJson = encodeToText $ ChooseDynasty dynasty

type DynastyMap = Map Dynasty Player

data SetupMessage = TakePosition Player Dynasty | Start deriving (Generic, Show)

setupMessage :: Player -> PlayerSetupMessage -> SetupMessage
setupMessage player (ChooseDynasty dynasty) = TakePosition player dynasty 
setupMessage _ StartGame = Start

data PlayerSetupMessage = ChooseDynasty Dynasty | StartGame deriving (Generic, Show)

instance FromJSON PlayerSetupMessage

instance ToJSON PlayerSetupMessage
