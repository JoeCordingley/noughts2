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
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Random.Lazy
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Lib
import Lucid (term, toHtml)
import Lucid.Base (Html)
import Lucid.Html5
import Tigris.Game
import Data.Aeson (FromJSON, ToJSON)
import Servant (Server)
import Api (GameApi, actionsApi, responses, paths)

gameServer :: IO (Server GameApi)
gameServer = actionsApi (responses $ paths Tigris) <$> tigrisActions

tigrisActions :: IO Actions
tigrisActions = actions (newGame openingState) hostGame (table $ flip gameHtml) <$> newTVarIO Map.empty
  where
    hostGame game = play game =<< evalRandIO . setupGame =<< seatPlayers game

openingState :: GameState
openingState = SeatingPlayers Map.empty

seatPlayers :: GameTVars GameState PlayerSetupMessage -> IO DynastyMap
seatPlayers game = updateWithNotify (seatPlayersUnfixed (uncurry setupMessage <$> nextMessageFromAnyPlayer (playerInputs game)) (pure . pure)) (notify game . SeatingPlayers) Map.empty

seatPlayersUnfixed :: (Monad m) => m SetupMessage -> (DynastyMap -> m (f DynastyMap)) -> (DynastyMap -> m (f DynastyMap)) -> DynastyMap -> m (f DynastyMap)
seatPlayersUnfixed receive end recurse playerMap = do
  message <- receive
  case message of
    TakePosition player position -> recurse $ takePosition playerMap
      where
        takePosition = if Map.notMember position playerMap then Map.insert position player . Map.filter (/= player) else id
    Start -> (if atLeastTwo playerMap then end else recurse) playerMap

atLeastTwo :: Map Dynasty a -> Bool
atLeastTwo playerMap = Map.size playerMap >= 2

data GameResult

play :: GameTVars GameState a -> PlayingState -> IO ()
play game = void . updateWithNotify (playGame (pure . pure)) (notify game . Playing)

getTurn :: Dynasty -> Game -> f (Either Pass Game)
getTurn = undefined

data GameState = SeatingPlayers DynastyMap | Playing PlayingState deriving (Show)

gameHtml :: GameState -> Player -> Html ()
gameHtml (SeatingPlayers map) = chooseDynasty map
gameHtml (Playing dynasties) = boardHtml dynasties

boardHtml :: PlayingState -> Player -> Html ()
boardHtml (PlayingState {_turnOrder}) player = div_ [id_ "board"] $ forM_ _turnOrder dynastyDiv
  where
    dynastyDiv dynasty = div_ $ toHtml $ show dynasty

chooseDynasty :: DynastyMap -> Player -> Html ()
chooseDynasty playerMap thisPlayer =
  div_ [id_ "board"] $ do
    h2_ "Choose Your Dynasty"
    div_ [class_ "dynasty-grid"]
      $ forM_ [Archer, Bull, Pot, Lion]
      $ dynastyDiv
    when (atLeastTwo playerMap) $ button_ [class_ "start-game action", term "hx-vals" jsonVal, term "ws-send" mempty] "Start Game"
  where
    jsonVal = encodeToText StartGame
    dynastyDiv :: Dynasty -> Html ()
    dynastyDiv dynasty = div_ ([class_ "dynasty-box"] ++ if isMine then [class_ "mine"] else []) $ do
      strong_ . toHtml $ show dynasty
      small_ $ toHtml status
      span_ [class_ "button-area"] $ unless isTaken $ button_ [class_ "dynasty action", term "hx-vals" jsonVal, term "ws-send" mempty] "Choose"
      where
        (isTaken, isMine, status) = case Map.lookup dynasty playerMap of
          Just player -> (True, player == thisPlayer, "Player: " <> playerName player)
          Nothing -> (False, False, "Available")
        jsonVal = encodeToText $ ChooseDynasty dynasty

type DynastyMap = Map Dynasty Player

data SetupMessage = TakePosition Player Dynasty | Start deriving (Generic, Show)

setupMessage :: Player -> PlayerSetupMessage -> SetupMessage
setupMessage player (ChooseDynasty a) = TakePosition player a
setupMessage _ StartGame = Start

data PlayerSetupMessage = ChooseDynasty {position :: Dynasty} | StartGame deriving (Generic, Show)

instance FromJSON PlayerSetupMessage

instance ToJSON PlayerSetupMessage
