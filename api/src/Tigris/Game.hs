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
    Action (..),
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
import Control.Applicative ((<|>))


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
getTurn getAction dynasty game' = flip applyAction game' <$> getAction dynasty game' where
  applyAction Pass = Left 
  applyAction (ReplaceTiles discards) = Right . (orDetermineWinners . playerAndBag $ state (at dynasty . traverse . hand) $ dealUpToSix . (<>) discards)
  applyAction (PositionLeader sphere leaderPosition) = Right . resolveRevolts . (over board $ placeLeader (Leader dynasty sphere) leaderPosition) where
    resolveRevolts :: Game -> Either Winners Game
    resolveRevolts game = case leaderPosition of
      Nothing -> Right game
      Just position -> case opposingLeader' $ view (board . grid ) game of
        Just opposingDynasty -> resolveRevolt opposingDynasty
        Nothing -> Right game
      where
        opposingLeader' grid = leaderPosition >>= flip (opposingLeader sphere) grid
  applyAction (PlaceTile sphere position) = Right . Right . (set (board . grid . at position . traverse . placedPiece) . Just $ PlacedTile sphere)

resolveRevolt :: Dynasty -> Either Winners Game
resolveRevolt = undefined

opposingLeader :: Sphere -> Position -> Grid -> Maybe Dynasty
opposingLeader sphere position = firstJust matchingLeader . view (at position . traverse . leaders)

firstJust :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
firstJust f = foldr ((<|>) . f) Nothing

matchingLeader :: Leader -> Maybe Dynasty
matchingLeader = undefined

view2 :: ((a -> Const a b) -> s -> Const a t) -> s -> a
view2 l = getConst . l Const 


x :: Monoid a => [a] -> a
x = view2 traverse

placeLeader :: Leader -> Maybe Position -> Board -> Board
placeLeader leader newPosition = uncurry (over grid . moveLeader )  . (leaderPositions . at leader) (, newPosition) where
  moveLeader oldPosition = maybe id addToGrid newPosition . maybe id removeFromGrid oldPosition
  removeFromGrid position = set (at position . traverse . placedPiece)  Nothing
  addToGrid position = set (at position. traverse . placedPiece) . Just $ PlacedLeader leader

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
