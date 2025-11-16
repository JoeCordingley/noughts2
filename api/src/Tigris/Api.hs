{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tigris.Api (Dynasty, gameServerDependencies) where

import BasicPrelude
import Control.Concurrent.STM (STM, TVar, newTVarIO, orElse, readTVar, retry)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Lib
import Lucid (term, toHtml)
import Lucid.Base (Html)
import Lucid.Html5

-- Types

data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)

instance FromJSON Dynasty

instance ToJSON Dynasty

gameServerDependencies :: GameServerDependencies
gameServerDependencies = GameServerDependencies Tigris tigrisActions

tigrisActions :: IO Actions
tigrisActions = actions (newGame openingState) (setupGame >=> play) (table chooseDynasty) <$> newTVarIO openingState
  where
    openingState = Map.empty

setupGame :: GameTVars DynastyMap PlayerSetupMessage -> IO DynastyMap
setupGame game = seatPlayers game (setup game) Map.empty

setup :: GameTVars DynastyMap PlayerSetupMessage -> (DynastyMap -> STMIO DynastyMap) -> DynastyMap -> STMIO DynastyMap
setup game = setupGameUnfixed (receiveMsg game) (pure . pure)

setupGameUnfixed :: (Monad m) => m SetupMessage -> (DynastyMap -> m (f DynastyMap)) -> (DynastyMap -> m (f DynastyMap)) -> DynastyMap -> m (f DynastyMap)
setupGameUnfixed receive end recurse playerMap = do
  message <- receive
  case message of
    TakePosition player position -> recurse $ takePosition playerMap
      where
        takePosition = if Map.notMember position playerMap then Map.insert position player . Map.filter (/= player) else id
    Start -> (if atLeastTwo playerMap then end else recurse) playerMap

type STMIO a = STM (IO a)

receiveMsg :: GameTVars DynastyMap PlayerSetupMessage -> STM SetupMessage
receiveMsg game = readTVar (playerInputs game) >>= nextPlayerMessage
  where
    nextPlayerMessage = foldr orElse retry . map (uncurry $ fmap . setupMessage)

atLeastTwo :: Map Dynasty a -> Bool
atLeastTwo playerMap = Map.size playerMap >= 2

play :: Map Dynasty Player -> IO ()
play m = putStrLn $ "playing: " <> tshow m

chooseDynasty :: Player -> DynastyMap -> Html ()
chooseDynasty thisPlayer playerMap =
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
