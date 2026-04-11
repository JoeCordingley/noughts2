{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Tigris.Data
  ( Sphere (..),
    Tile (..),
    tileSphere,
    Dynasty (..),
    Leader (..),
    leaderSphere,
    PlayingState (..),
    turnOrder,
    game,
    PlayerInfos,
    Game (..),
    bag,
    players,
    leaders,
    board,
    Board (..),
    numberOfTreasuresLeft,
    leaderPositions,
    grid,
    Grid,
    LeaderPositions,
    PlayerInfo (..),
    score,
    hand,
    catastropheTiles,
    Space (..),
    marking,
    placedPiece,
    PlacedPiece (..),
    Marking (..),
    Bag,
    Hand,
    Score,
    Winners,
    Action (..),
    Position (..),
  )
where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Monoid (Sum)

data Sphere = Temples | Markets | Settlements | Farms deriving (Show, Eq, Ord)
newtype Tile = Tile {_tileSphere :: Sphere} deriving (Show, Eq, Ord)
data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)
instance FromJSON Dynasty
instance ToJSON Dynasty

data Leader = Leader {_leaderDynasty :: Dynasty, _leaderSphere :: Sphere} deriving (Show, Eq, Ord)

data PlayingState = PlayingState {_turnOrder :: [Dynasty], _game :: Game} deriving (Show)

type PlayerInfos = Map Dynasty PlayerInfo

data Game = Game {_bag :: [Tile], _players :: Map Dynasty PlayerInfo, _board :: Board} deriving (Show)

data Board = Board {_numberOfTreasuresLeft :: Int, _leaderPositions :: LeaderPositions, _grid :: Grid } deriving Show

type Grid = Map Position Space

type LeaderPositions = Map Leader Position

data PlayerInfo = PlayerInfo {_score :: Score, _hand :: Bag Tile, _catastropheTiles :: Int} deriving Show

data Space = Space {_marking :: Marking, _placedPiece :: Maybe PlacedPiece, _leaders :: [Leader]} deriving Show

data PlacedPiece = PlacedLeader Leader | PlacedTile Sphere deriving Show

data Marking = Sand | River | Temple {specialBorder :: Bool} deriving Show

type Bag a = Map a (Sum Int)

type Hand = Bag Tile

type Score = Map Sphere (Sum Int)

type Winners = [Dynasty]

data Action = PositionLeader Sphere (Maybe Position) | PlaceTile Sphere Position | PlayCatastrophe | ReplaceTiles Hand | Pass
data Position = Position deriving (Show, Eq, Ord)

makeLenses ''Tile
makeLenses ''Leader
makeLenses ''PlayingState
makeLenses ''Game
makeLenses ''Board
makeLenses ''PlayerInfo
makeLenses ''Space
