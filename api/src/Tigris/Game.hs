{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Tigris.Game
  ( Dynasty (..),
    PlayingState (..),
    Game (..),
    Board (..),
    Grid,
    LeaderPositions,
    PlayerInfo (..),
    Space (..),
    PlacedPiece (..),
    Marking (..),
    Bag,
    Hand,
    Score,
    Winners,
    Pass,
    Action (..),
    LeaderPosition (..),
    Position (..),
    setupGame,
    playTurn,
    playGame,
  )
where

import Control.Lens
import Control.Monad.Random.Lazy
import Control.Arrow (first)
import Control.Monad.Except (ExceptT (..), runExceptT, liftEither)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random.Shuffle (shuffleM)
import Data.Bool (bool)
import Data.List (sort)
import Control.Monad.State (StateT(..), runStateT)
import Data.Monoid (Sum(..))
import Data.Maybe (fromJust)
import Tigris.Data


startingPlayerInfo :: Hand -> PlayerInfo
startingPlayerInfo startingHand = PlayerInfo{_score = startingScore, _hand = startingHand, _catastropheTiles = 2}


one :: a -> Bag a 
one a = Map.singleton a (Sum 1)

startingBoard :: Board
startingBoard = Board {_numberOfTreasuresLeft = 10, _leaderPositions = Map.empty, _grid = Map.empty}

allCivilizationTiles :: Bag Tile
allCivilizationTiles = Map.fromList $ map (first Tile) [(Temples, 57), (Markets, 30), (Settlements, 30), (Farms,  36)]

tilesMinusStartingTemples :: Bag Tile
tilesMinusStartingTemples = allCivilizationTiles <> Map.singleton (Tile Temples) (-10)

bagToList :: Bag a -> [a]
bagToList = Map.foldrWithKey f [] where
  f k (Sum n) ts = replicate n k <> ts


emptyHand :: Hand
emptyHand = Map.fromList $ map (first Tile) allSpheresZero

setupGame :: (MonadRandom m) => Map Dynasty a -> m PlayingState
setupGame m = fromShuffled <$> shuffleM dynasties <*> shuffleM (bagToList tilesMinusStartingTemples)
  where
    fromShuffled dynasties' tiles = PlayingState {_turnOrder = cycle dynasties, _game = startingGame} where
      startingGame = Game {_bag = remainingTiles, _players = fmap startingPlayerInfo startingPlayerHand, _board = startingBoard}
      (startingPlayerHand, remainingTiles) = fromJust $ state traverse dealUpToSix (emptyHands, tiles)
      emptyHands = Map.fromList $ map (,emptyHand) dynasties'
    dynasties = Map.keys m

winners :: Map Dynasty Score -> Winners
winners finalScores' = snd . Map.findMax $ Map.foldMapWithKey groupByScore finalScores' where
  groupByScore k v = Map.singleton (sort $ Map.elems v) [k]


allSpheresZero :: [(Sphere, Sum Int)]
allSpheresZero = map (,0) spheres

startingScore :: Score
startingScore = Map.fromList $ allSpheresZero

spheres :: [Sphere]
spheres = [Settlements, Temples, Farms, Markets]

playGame :: Monad m => (Winners -> m a) -> (PlayingState -> m a) -> PlayingState -> m a
playGame finish recurse (PlayingState (player : subsequentPlayers) game') = playTurn (getTurn getAction) player game' >>= either finish (recurse . PlayingState subsequentPlayers)


getAction :: Dynasty -> Game -> f Action
getAction = undefined

playTurn :: (Monad f) => (Dynasty -> Game -> f (Either Game (Either Winners Game))) -> Dynasty -> Game -> f (Either Winners Game)
playTurn getTurn player = runExceptT . (liftEither . (maybeEndGame <=< endTurn) <=< playUpToTwoTurns) where
  playUpToTwoTurns = orReturnAfterPass . twice turn
  orReturnAfterPass = ExceptT . fmap (either Right id) . runExceptT . runExceptT
  turn = ExceptT . ExceptT . getTurn player

twice :: Monad f => (a -> f a) -> (a -> f a)
twice f = f >=> f

orDetermineWinners :: (Game -> Maybe Game) -> Game -> Either Winners Game
orDetermineWinners f game' = maybe (Left . winners . fmap (view score) $ view players game') Right $ f game'

maybeEndGame :: Game -> Either Winners Game
maybeEndGame = orDetermineWinners continueGame

continueGame :: Game -> Maybe Game
continueGame game' = bool Just (const Nothing) (isFinished $ view board game') game'

isFinished :: Board -> Bool
isFinished board' = view numberOfTreasuresLeft board' <= 2

getTurn :: Functor f => (Dynasty -> Game -> f Action) -> Dynasty -> Game -> f (Either Game (Either Winners Game))
getTurn f dynasty game' = flip g game' <$> f dynasty game' where
  g Pass = Left 
  g (ReplaceTiles hand') = Right . (orDetermineWinners . playerAndBag $ state (at dynasty . traverse . hand) dealUpToSix)
  g (PositionLeader leader leaderPosition) = (Right . Right) . case leaderPosition of
    OffBoard -> over board (placeLeaderOffBoard dynasty leader Nothing)
    OnBoard position -> undefined

placeLeaderOffBoard :: Dynasty -> Leader -> Maybe Position -> Board -> Board
placeLeaderOffBoard dynasty leader newPosition = uncurry (over grid . moveLeader )  . (leaderPositions . at (dynasty, leader)) (, newPosition) where
  moveLeader oldPosition = maybe id addToGrid newPosition . maybe id removeFromGrid oldPosition
  removeFromGrid position = over (at position) (const Nothing)
  addToGrid position = over (at position) undefined

endTurn :: Game -> Either Winners Game
endTurn = orDetermineWinners . playerAndBag $ state (traverse . hand) dealUpToSix

state :: ((a -> StateT st f b) -> s -> StateT st f t) -> (a -> StateT st f b) -> (s, st) -> f (t, st)
state l f (s, st) = runStateT (l f s) st

dealUpToSix :: Hand -> StateT [Tile] Maybe Hand
dealUpToSix playerTiles = foldr (>=>) pure (replicate (6 - length playerTiles) (StateT . dealOne)) playerTiles where
  dealOne playerTiles' (x:xs) = Just (one x <> playerTiles', xs)
  dealOne _ [] = Nothing

playerAndBag :: Functor f => ((PlayerInfos, [Tile]) -> f (PlayerInfos, [Tile])) -> Game -> f Game
playerAndBag f game' = (\(_players, _bag) -> game'{_players, _bag}) <$> f (_players game', _bag game')
