{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Tigris.Game (Dynasty(..), PlayingState(..), Game, setupGame, playTurn, playGame, Pass) where

import Control.Monad.Random.Lazy
import Control.Monad.Except (ExceptT (..), runExceptT, liftEither)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import System.Random.Shuffle (shuffleM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bool (bool)
import Data.List (sort)
import Control.Monad.State (StateT(..))
import Data.Monoid (Sum(..))

type Sphere = Tile

data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)
instance FromJSON Dynasty

instance ToJSON Dynasty

data PlayingState = PlayingState {turnOrder :: [Dynasty], game :: Game} deriving (Show)

type PlayerInfos = Map Dynasty PlayerInfo

data Game = Game {bag :: [Tile], players :: Map Dynasty PlayerInfo, board :: Board} deriving (Show)

data Board = Board {numberOfTreasuresLeft :: Int} deriving Show

data PlayerInfo = PlayerInfor {score :: Score, hand :: Bag Tile} deriving Show

type Bag a = Map a (Sum Int)

type Hand = Bag Tile

one :: a -> Bag a 
one a = Map.singleton a (Sum 1)

data Tile = Temple | Market | Settlement | Farm deriving (Show, Eq, Ord)

startingBoard :: Board
startingBoard = undefined

allCivilizationTiles :: Bag Tile
allCivilizationTiles = Map.fromList[(Temple, 57), (Market, 30), (Settlement, 30), (Farm,  36)]

tilesMinusStartingTemples :: Bag Tile
tilesMinusStartingTemples = allCivilizationTiles <> Map.singleton Temple (-10)

bagToList :: Bag a -> [a]
bagToList = Map.foldrWithKey f [] where
  f k (Sum n) ts = replicate n k <> ts

type Score = Map Sphere Int

type Scores = Map Dynasty Score

setupGame :: (MonadRandom m) => Map Dynasty a -> m PlayingState
setupGame m = fromShuffled <$> shuffleM dynasties <*> shuffleM (bagToList tilesMinusStartingTemples)
  where
    fromShuffled dynasties' tiles = PlayingState {turnOrder = cycle dynasties, game = startingGame} where
      startingGame = Game {players = Map.fromList $ map (,startingPlayerInfo) dynasties', board = startingBoard}
    dynasties = Map.keys m

type Winners = [Dynasty]

winners :: Map Dynasty Score -> Winners
winners finalScores' = snd . Map.findMax $ Map.foldMapWithKey groupByScore finalScores' where
  groupByScore k v = Map.singleton (sort $ Map.elems v) [k]

data Pass

startingPlayerInfo :: PlayerInfo
startingPlayerInfo = undefined

startingScore :: Score
startingScore = Map.fromList $ map (,0) spheres

spheres :: [Sphere]
spheres = [Settlement, Temple, Farm, Market]

playGame :: Monad m => (Winners -> m a) -> (PlayingState -> m a) -> PlayingState -> m a
playGame finish recurse (PlayingState (player : subsequentPlayers) game) = playTurn getTurn player game >>= either finish (recurse . PlayingState subsequentPlayers)

getTurn :: Dynasty -> Game -> f (Either Game (Either Winners Game))
getTurn = undefined

playTurn :: (Monad f) => (Dynasty -> Game -> f (Either Game (Either Winners Game))) -> Dynasty -> Game -> f (Either Winners Game)
playTurn getTurn player = runExceptT . (liftEither . maybeEndGame <=< liftEither . endTurn <=< playUpToTwoTurns) where
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

endTurn :: Game -> Either Winners Game
endTurn = orDetermineWinners . playerAndBag . uncurry $ deals dealUpToSix where

dealUpToSix :: Hand -> [Tile] -> Maybe (Hand, [Tile])
dealUpToSix playerTiles bag = foldr (>=>) pure (replicate (6 - length playerTiles) dealOne) (playerTiles, bag) where
  dealOne (playerTiles, x:xs) = Just (one x <> playerTiles, xs)
  dealOne (_,[]) = Nothing

deals :: (Monad m) => (Hand -> [Tile] -> m (Hand, [Tile])) -> PlayerInfos -> [Tile] -> m (PlayerInfos, [Tile])
deals f = runStateT . (traverse . playerHand $ StateT . f)

playerAndBag :: Functor f => ((PlayerInfos, [Tile]) -> f (PlayerInfos, [Tile])) -> Game -> f Game
playerAndBag f game = uncurry g <$> f (players game, bag game) where
  g players bag = game{players, bag}

playerHand :: Functor f => (Hand -> f Hand) -> PlayerInfo -> f PlayerInfo
playerHand f playerInfo = fmap g . f $ hand playerInfo where
  g hand = playerInfo{hand}
