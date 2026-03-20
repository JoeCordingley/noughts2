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

data Sphere = Settlements | Temples | Farms | Markets deriving (Show, Eq, Ord)

data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)
instance FromJSON Dynasty

instance ToJSON Dynasty

data PlayingState = PlayingState {turnOrder :: [Dynasty], game :: Game} deriving (Show)

type PlayerInfos = Map Dynasty PlayerInfo

data Game = Game {bag :: [Tile], players :: Map Dynasty PlayerInfo, board :: Board} deriving (Show)

data Board = Board {numberOfTreasuresLeft :: Int} deriving Show

data PlayerInfo = PlayerInfor {score :: Score, hand :: [Tile]} deriving Show

data Tile = Tile deriving Show

startingBoard :: Board
startingBoard = undefined

type Score = Map Sphere Int

type Scores = Map Dynasty Score

setupGame :: (MonadRandom m) => Map Dynasty a -> m PlayingState
setupGame m = fromShuffled <$> shuffleM dynasties
  where
    fromShuffled dynasties' = PlayingState {turnOrder = cycle dynasties, game = startingGame} where
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
spheres = [Settlements, Temples, Farms, Markets]

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
  dealUpToSix playerTiles bag = foldr (>=>) pure (replicate (6 - length playerTiles) dealOne) (playerTiles, bag) where
    dealOne (playerTiles, x:xs) = Just (x:playerTiles, xs)
    dealOne (_,[]) = Nothing
  deals f = runStateT . (traverse . playerHand $ StateT . f)

gamePlayers :: Functor f => (PlayerInfos -> f PlayerInfos) -> Game -> f Game
gamePlayers f game = g <$> f (players game) where
  g playerInfos = game {players = playerInfos}

playerAndBag :: Functor f => ((PlayerInfos, [Tile]) -> f (PlayerInfos, [Tile])) -> Game -> f Game
playerAndBag f game = uncurry g <$> f (players game, bag game) where
  g players bag = game{players, bag}

playerHand :: Functor f => ([Tile] -> f [Tile]) -> PlayerInfo -> f PlayerInfo
playerHand f playerInfo = fmap g . f $ hand playerInfo where
  g hand = playerInfo{hand}
