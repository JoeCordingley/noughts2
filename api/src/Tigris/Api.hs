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
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import qualified Data.Text as T
import Servant (Server)
import Api (GameApi, actionsApi, responses, paths)
import Control.Lens
import qualified Data.Array as Array
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Monoid (Sum(..))

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
openingState = SeatingPlayers Bimap.empty

seatPlayers :: GameTVars GameState -> DynastyMap -> IO DynastyMap
seatPlayers game = fix (recursing (atomically . notify game . SeatingPlayers) . seatPlayersUnfixed (fmap (uncurry setupMessage) . nextValidMessageFromAnyPlayer $ playerInputs game))

seatPlayersUnfixed :: (Monad m) => m SetupMessage -> (DynastyMap -> m DynastyMap) -> DynastyMap -> m DynastyMap
seatPlayersUnfixed receive recurse playerMap = do
  message <- receive
  case message of
    TakePosition player position -> recurse $ takePosition playerMap
      where
        takePosition = if Bimap.notMember position playerMap then Bimap.insert position player . Bimap.deleteR player else id
    Start -> (if atLeastTwo playerMap then pure else recurse) playerMap

atLeastTwo :: Bimap Dynasty a -> Bool
atLeastTwo playerMap = Bimap.size playerMap >= 2

data GameResult

play :: GameTVars GameState -> DynastyMap -> PlayingState -> IO ()
play game dynasties playingState = do
  inputs <- dynastyInputs game dynasties 
  void $ fix (recursing (atomically . notify game . Playing dynasties) . playGame (interactions inputs)) playingState

dynastyInputs :: GameTVars GameState -> DynastyMap -> IO (Map Dynasty (IO ByteString))
dynastyInputs game dynastyPlayers = inputMap <$> readTVarIO (playerInputs game) where
  inputMap m = Map.map (atomically . (m !)) $ Bimap.toMap dynastyPlayers 

data GameState = SeatingPlayers DynastyMap | Playing DynastyMap PlayingState deriving (Show)

gameHtml :: GameState -> Player -> Html ()
gameHtml (SeatingPlayers m) = chooseDynasty m
gameHtml (Playing m playingState) = playingHtml . htmlModel m playingState

data HtmlModel = HtmlModel {htmlGrid :: Grid, playerModel :: Maybe PlayerHtmlModel }
data PlayerHtmlModel = PlayerHtmlModel {isCurrentPlayer :: Bool, leadersInHand :: Set Sphere, tilesInHand :: Map Sphere Int, playerScore :: Map ScoreArea Int}

htmlModel :: DynastyMap -> PlayingState -> Player -> HtmlModel
htmlModel m (PlayingState (GameStage dynasties _) game) player = case dynasties of
  (currentPlayer:_) -> HtmlModel { htmlGrid = (view (board . grid) game), playerModel } where
    playerModel = do
      playerDynasty <- Bimap.lookupR player m
      playerInfo <- view (players . at playerDynasty) game
      return $ PlayerHtmlModel {isCurrentPlayer = playerDynasty == currentPlayer, leadersInHand = view playerLeadersInHand playerInfo, tilesInHand = fmap getSum $ view hand playerInfo, playerScore = fmap getSum $ view score playerInfo}
  _ -> undefined


xIfTemplate :: Text -> Html () -> Html ()
xIfTemplate predicate = template_ [term "x-if" predicate] . div_

wsSend :: Attributes
wsSend = term "ws-send" mempty

hxVals :: Text -> Attributes
hxVals = term "hx-vals"

emptyDiv :: [Attributes] -> Html ()
emptyDiv attrs = div_ attrs mempty

jsLines :: [Text] -> Text
jsLines = intercalate "; "

assignments :: [(Text, Text)] -> Text
assignments = jsLines . map (uncurry assignment)

assignment :: Text -> Text -> Text
assignment x y = x <> " = " <> y

quoted :: Text -> Text 
quoted x = "'" <> x <> "'"

playingHtml :: HtmlModel -> Html ()
playingHtml (HtmlModel {htmlGrid, playerModel}) = gameDiv $ div_ [xData "{ action: null, sphere: null }"] $ do
  xIfTemplate "action == 'leader'" $ div_ $ span_ [term "x-text" "sphere"] mempty <> " leader selected"
  boardHtml $ htmlGrid
  traverse_ playerInfo' playerModel 
  where
    playerInfo' :: PlayerHtmlModel -> Html ()
    playerInfo' (PlayerHtmlModel{isCurrentPlayer, leadersInHand, tilesInHand, playerScore}) = do
       when isCurrentPlayer $ "your turn"
       div_ [class_ "leader-area"] $ do
         "leaders"
         div_ [class_ "leaders"] $ traverse_ (emptyDiv . leaderAttributes isCurrentPlayer) leadersInHand
       div_ [class_ "tile-area"] $ do
         "tiles"
         div_ [class_ "tiles"] $ traverse_ (if isCurrentPlayer then uncurry tileButton else uncurry tileNonButton) $ Map.toList tilesInHand
       div_ [class_ "score-area"] $ do
         "scores"
         div_ [class_ "scores"] $ traverse_ (uncurry scoreDiv) $ Map.toList playerScore
    scoreDiv :: ScoreArea -> Int -> Html ()
    scoreDiv (SphereScore sphere) = numberedSquare $ sphereText sphere 
    scoreDiv Treasure = numberedSquare "treasure"
    leaderAttributes :: Bool -> Sphere -> [Attributes]
    leaderAttributes isCurrentPlayer sphere = (guard isCurrentPlayer *> [term "@click" $ assignments [("action", "'leader'"),("sphere", quoted $ sphereText sphere)]]) <> [classes_ ["piece", "leader", sphereText sphere]]
    numberedSquare :: Text -> Int -> Html ()
    numberedSquare t i = div_ [classes_ ["piece", "tile", t, "hand"]] $ toHtml $ show i
    tileNonButton :: Sphere -> Int -> Html ()
    tileNonButton sphere = numberedSquare $ sphereText sphere
    tileButton :: Sphere -> Int -> Html ()
    tileButton s 0 = tileNonButton s 0
    tileButton s i = tileNonButton s i
    boardHtml g = do
      xIfTemplate "!action" $ boardDiv $ traverse_ inactiveSquare g
      xIfTemplate "action == 'leader'" $ do
        xIfTemplate "sphere == 'temples'" $ leaderBoard Temples
        xIfTemplate "sphere == 'settlements'" $ leaderBoard Settlements
        xIfTemplate "sphere == 'farms'" $ leaderBoard Farms
        xIfTemplate "sphere == 'markets'" $ leaderBoard Markets
      where 
      leaderBoard sphere = boardDiv $ traverse_ (uncurry $ maybeLeaderSquare sphere) $ Array.assocs g
    maybeLeaderSquare sphere position s = bool (inactiveSquare s) (leaderSquare (encodeToText . PositionLeader sphere $ Just position) s) (view nextToTemples s)
    boardDiv = div_ [id_ "board", term "x-init" "htmx.process($el)"] 
    piece' = traverse_ pieceHtml 
    inactiveSquare s = div_ ([classes_ $ ["tigris-square", markingText $ view marking s]] ) . piece' $ view slot s
    leaderSquare hxVals' s = div_ ([classes_ $ ["tigris-square", markingText $ view marking s, "clickable"], wsSend, hxVals hxVals'] ) . piece' $ view slot s
    pieceHtml :: Piece -> Html ()
    pieceHtml (TilePiece sphere) = emptyDiv [classes_ ["piece", sphereText sphere, "tile"]] 
    pieceHtml (LeaderPiece dynasty sphere) = emptyDiv [classes_ ["piece", sphereText sphere, "leader", dynastyText dynasty]] 

interactions :: Map Dynasty (IO ByteString) -> Dynasty -> Interactions IO 
interactions m dynasty = Interactions {getCommittedTemples = getValidMessage (m ! dynasty), getAction = getValidMessage (m ! dynasty)} where

getValidMessage :: FromJSON a => IO ByteString -> IO a
getValidMessage fb = either logAndRetry pure . eitherDecodeStrict =<< fb where
 logAndRetry e = putStrLn (T.pack e) *> getValidMessage fb

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
        (isTaken, isMine, status) = case Bimap.lookup dynasty playerMap of
          Just player -> (True, player == thisPlayer, "Player: " <> playerName player)
          Nothing -> (False, False, "Available")
        chooseDynastyJson = encodeToText $ ChooseDynasty dynasty

type DynastyMap = Bimap Dynasty Player

data SetupMessage = TakePosition Player Dynasty | Start deriving (Generic, Show)

setupMessage :: Player -> PlayerSetupMessage -> SetupMessage
setupMessage player (ChooseDynasty dynasty) = TakePosition player dynasty 
setupMessage _ StartGame = Start

data PlayerSetupMessage = ChooseDynasty Dynasty | StartGame deriving (Generic, Show)

instance FromJSON PlayerSetupMessage

instance ToJSON PlayerSetupMessage

