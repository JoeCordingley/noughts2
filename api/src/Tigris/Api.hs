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
play game dynasties = void . fix (recursing (atomically . notify game . Playing dynasties) . playGame )

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
xIfTemplate predicate = template_ [term "x-if" predicate] 

playingHtml :: HtmlModel -> Html ()
playingHtml (HtmlModel {htmlGrid, isCurrentPlayer, leadersInHand}) = gameDiv $ div_ [xData "{ action: null }"] $ do
  bool "not your turn" "your turn" isCurrentPlayer
  xIfTemplate "action" $ div_ $ span_ [term "x-text" "sphere"] mempty <> " selected"
  boardHtml $ htmlGrid
  when isCurrentPlayer $ traverse_ leaderButton leadersInHand
  where
    leaderButton :: Sphere -> Html ()
    leaderButton sphere = button_ [term "@click" ("action = 'leader'; sphere = '" <> sphereText' <> "'") ] $ toHtml sphereText' where
      sphereText' = sphereText sphere
    boardHtml = div_ [id_ "board"] . traverse_ square 
    pieceHtml :: Piece -> Html ()
    pieceHtml (TilePiece sphere) = div_ [classes_ ["piece", pieceType]] $ mempty where 
      pieceType = case sphere of
        Temples -> "temple"
        _ -> "x"
    pieceHtml (LeaderPiece _) = undefined
    square :: Space -> Html ()
    square s = div_ [classes_ ["tigris-square", marking']] $ piece' where
      marking' = case view marking s of
        Sand -> "sand"
        River -> "river"
      piece' = traverse_ pieceHtml (view slot s) 
--    boardHtml grid' = div_ [id_ "grid"] $
--      for_ [rowMin..rowMax] $ \r -> 
--        for_ [columnMin..columnMax] $ \c -> 
--            where
--              marking' = case (view (at (r, c) . marking) grid) of
--              Temple -> "temple"
--              Sand -> "sand"
--              River -> "river"

classes_ :: [Text] -> Attributes
classes_ = class_ . intercalate " "

chooseDynasty :: DynastyMap -> Player -> Html ()
chooseDynasty playerMap thisPlayer =
  gameDiv $ do
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
      span_ [class_ "button-area"] $ unless isTaken $ button_ [class_ "dynasty action", term "hx-vals" chooseDynastyJson, term "ws-send" mempty] "Choose"
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
