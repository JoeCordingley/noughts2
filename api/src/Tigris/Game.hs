{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Tigris.Game (Dynasty(..), PlayingState(..), Game, setupGame, playTurn, playGame, Pass) where

import Control.Monad.Random.Lazy
import Control.Arrow (first)
import Control.Monad.Except (ExceptT (..), runExceptT, liftEither)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random.Shuffle (shuffleM)
import Data.Bool (bool)
import Data.List (sort)
import Control.Monad.State (StateT(..))
import Data.Monoid (Sum(..))
import Data.Maybe (fromJust)
import Data.Functor.Identity (Identity(..))
import Tigris.Data (Sphere(..), Tile(..), Dynasty(..), Leader(..))


data PlayingState = PlayingState {turnOrder :: [Dynasty], game :: Game} deriving (Show)

type PlayerInfos = Map Dynasty PlayerInfo

data Game = Game {bag :: [Tile], players :: Map Dynasty PlayerInfo, board :: Board} deriving (Show)

data Board = Board {numberOfTreasuresLeft :: Int, leaderPositions :: LeaderPositions, grid :: Grid } deriving Show

type Grid = Map Position Space

type LeaderPositions = Map (Dynasty, Leader) Position


data PlayerInfo = PlayerInfo {score :: Score, hand :: Bag Tile, catastropheTiles :: Int} deriving Show

data Space = Space {marking :: Marking, placedPiece :: Maybe PlacedPiece} deriving Show

data PlacedPiece = PlacedLeader Leader deriving Show

data Marking = Sand | River | Temple {specialBorder :: Bool} deriving Show

startingPlayerInfo :: Hand -> PlayerInfo
startingPlayerInfo startingHand = PlayerInfo{score = startingScore, hand = startingHand, catastropheTiles = 2}

type Bag a = Map a (Sum Int)

type Hand = Bag Tile

one :: a -> Bag a 
one a = Map.singleton a (Sum 1)

boardLeaderPositions :: Functor f => (LeaderPositions -> f LeaderPositions) -> Board -> f Board
boardLeaderPositions f board = g <$> f (leaderPositions board) where
  g leaderPositions = board{leaderPositions}


startingBoard :: Board
startingBoard = Board {numberOfTreasuresLeft = 10}

allCivilizationTiles :: Bag Tile
allCivilizationTiles = Map.fromList $ map (first Tile) [(Temples, 57), (Markets, 30), (Settlements, 30), (Farms,  36)] where

tilesMinusStartingTemples :: Bag Tile
tilesMinusStartingTemples = allCivilizationTiles <> Map.singleton (Tile Temples) (-10)

bagToList :: Bag a -> [a]
bagToList = Map.foldrWithKey f [] where
  f k (Sum n) ts = replicate n k <> ts

type Score = Map Sphere (Sum Int)

emptyHand :: Hand
emptyHand = Map.fromList $ map (first Tile) allSpheresZero

setupGame :: (MonadRandom m) => Map Dynasty a -> m PlayingState
setupGame m = fromShuffled <$> shuffleM dynasties <*> shuffleM (bagToList tilesMinusStartingTemples)
  where
    fromShuffled dynasties' tiles = PlayingState {turnOrder = cycle dynasties, game = startingGame} where
      startingGame = Game {bag = remainingTiles, players = fmap startingPlayerInfo startingPlayerHand, board = startingBoard}
      (startingPlayerHand, remainingTiles) = fromJust $ state traverse dealUpToSix (emptyHands, tiles)
      emptyHands = Map.fromList $ map (,emptyHand) dynasties'
    dynasties = Map.keys m

type Winners = [Dynasty]

winners :: Map Dynasty Score -> Winners
winners finalScores' = snd . Map.findMax $ Map.foldMapWithKey groupByScore finalScores' where
  groupByScore k v = Map.singleton (sort $ Map.elems v) [k]

data Pass

allSpheresZero :: [(Sphere, Sum Int)]
allSpheresZero = map (,0) spheres

startingScore :: Score
startingScore = Map.fromList $ allSpheresZero

spheres :: [Sphere]
spheres = [Settlements, Temples, Farms, Markets]

playGame :: Monad m => (Winners -> m a) -> (PlayingState -> m a) -> PlayingState -> m a
playGame finish recurse (PlayingState (player : subsequentPlayers) game) = playTurn (getTurn getAction) player game >>= either finish (recurse . PlayingState subsequentPlayers)

data Action = PositionLeader Leader LeaderPosition | PlaceTile | PlayCatastrophe | ReplaceTiles Hand| Pass
data LeaderPosition = OffBoard | OnBoard Position
data Position = Position deriving (Show, Eq, Ord)

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
orDetermineWinners f game = maybe (Left . winners . fmap score $ players game) Right $ f game

maybeEndGame :: Game -> Either Winners Game
maybeEndGame = orDetermineWinners continueGame

continueGame :: Game -> Maybe Game
continueGame game = bool Just (const Nothing) (isFinished $ board game) game

isFinished :: Board -> Bool
isFinished board = numberOfTreasuresLeft board <= 2

getTurn :: Functor f => (Dynasty -> Game -> f Action) -> Dynasty -> Game -> f (Either Game (Either Winners Game))
getTurn f dynasty game = flip g game <$> f dynasty game where
  g Pass = Left 
  g (ReplaceTiles hand) = Right . (orDetermineWinners . playerAndBag $ state (at dynasty . traverse . playerHand)  dealUpToSix)
  g (PositionLeader leader leaderPosition) = Right . Right . case leaderPosition of
    OffBoard -> over gameBoard $ placeLeaderOffBoard dynasty leader Nothing
    OnBoard position -> undefined

placeLeaderOffBoard :: Dynasty -> Leader -> Maybe Position -> Board -> Board
-- placeLeaderOffBoard dynasty leader maybePosition = uncurry (maybe id removeFromGrid) . (boardLeaderPositions . at (dynasty, leader)) (, maybePosition) where
placeLeaderOffBoard dynasty leader newPosition = uncurry (over boardGrid . moveLeader )  . (boardLeaderPositions . at (dynasty, leader)) (, newPosition) where
  moveLeader oldPosition = maybe id addToGrid newPosition . maybe id removeFromGrid oldPosition
  removeFromGrid position = over (at position) (const Nothing) 
  addToGrid position = over (at position) undefined

gameBoard :: Functor f => (Board -> f Board) -> Game -> f Game
gameBoard f game = g <$> f (board game) where
  g board = game{board}

boardGrid :: Functor f => (Grid -> f Grid) -> Board -> f Board
boardGrid f board = g <$> f (grid board) where
  g grid = board{grid}

replace :: ((a -> (a, b)) -> s -> (a,t)) -> (a -> b) -> s -> (a, t)
replace l f = l g where
  g a = (a, f a)

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set l = over l . const 

endTurn :: Game -> Either Winners Game
endTurn = orDetermineWinners . playerAndBag $ state (traverse . playerHand) dealUpToSix

state :: ((a -> StateT st f b) -> s -> StateT st f t) -> (a -> StateT st f b) -> (s, st) -> f (t, st)
state l f (s, st) = runStateT (l f s) st

dealUpToSix :: Hand -> StateT [Tile] Maybe Hand
dealUpToSix playerTiles = foldr (>=>) pure (replicate (6 - length playerTiles) (StateT . dealOne)) playerTiles where
  dealOne playerTiles (x:xs) = Just (one x <> playerTiles, xs)
  dealOne _ [] = Nothing

playerAndBag :: Functor f => ((PlayerInfos, [Tile]) -> f (PlayerInfos, [Tile])) -> Game -> f Game
playerAndBag f game = uncurry g <$> f (players game, bag game) where
  g players bag = game{players, bag}

playerHand :: Functor f => (Hand -> f Hand) -> PlayerInfo -> f PlayerInfo
playerHand f playerInfo = fmap g . f $ hand playerInfo where
  g hand = playerInfo{hand}

at :: (Ord k, Functor f) => k -> (Maybe v -> f (Maybe v)) -> Map k v -> f (Map k v)
at d f = Map.alterF f d

