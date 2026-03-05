{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Tigris.Game where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Random.Lazy
import Data.Either (fromRight)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import System.Random.Shuffle (shuffleM)

data Sphere = Settlements | Temples | Farms | Markets deriving (Show, Eq, Ord)

data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)

data PlayingState = PlayingState {turnOrder :: [Dynasty], game :: Game} deriving (Show)

data Game = Game {scores :: Map Dynasty Score} deriving (Show)

type Score = Map Sphere Int

setupGame :: (MonadRandom m) => Map Dynasty a -> m PlayingState
setupGame m = fromShuffled <$> shuffleM dynasties
  where
    fromShuffled dynasties = PlayingState {turnOrder = cycle dynasties, game = Game (Map.fromList $ map (,startingScore) dynasties)}
    dynasties = Map.keys m

data FinalScore = FinalScore Score

data Pass = Pass

startingScore = Map.fromList $ map (,0) spheres

spheres :: [Sphere]
spheres = [Settlements, Temples, Farms, Markets]

playTurn :: (Monad f) => (Dynasty -> Game -> f (Either Pass Game)) -> PlayingState -> f (Either FinalScore PlayingState)
playTurn getTurn = nextPlayer $ playerTurn getTurn
  where
    nextPlayer playerTurn (PlayingState (player : subsequentPlayers) game) = fmap (PlayingState subsequentPlayers) <$> playerTurn player game

playerTurn :: (Monad f) => (Dynasty -> Game -> f (Either Pass Game)) -> Dynasty -> Game -> f (Either FinalScore Game)
playerTurn getTurn dynasty game = maybeEnd <$> playUpToTwoTurns
  where
    playUpToTwoTurns = maybeFinish . fromRight game <$> runExceptT (turn =<< turn game)
      where
        maybeFinish = undefined
        turn = ExceptT . getTurn dynasty
    maybeEnd = undefined
