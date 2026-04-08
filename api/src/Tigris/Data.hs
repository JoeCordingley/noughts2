{-# LANGUAGE DeriveGeneric #-}
module Tigris.Data
  ( Sphere (..),
    Tile (..),
    Dynasty (..),
    Leader (..),
    PlayingState (..),
    PlayerInfos,
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
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Monoid (Sum)

data Sphere = Temples | Markets | Settlements | Farms deriving (Show, Eq, Ord)
newtype Tile = Tile {tileSphere :: Sphere} deriving (Show, Eq, Ord)
data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)
instance FromJSON Dynasty
instance ToJSON Dynasty

newtype Leader = Leader {leaderSphere :: Sphere} deriving (Show, Eq, Ord)

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

type Bag a = Map a (Sum Int)

type Hand = Bag Tile

type Score = Map Sphere (Sum Int)

type Winners = [Dynasty]

data Pass

data Action = PositionLeader Leader LeaderPosition | PlaceTile | PlayCatastrophe | ReplaceTiles Hand | Pass
data LeaderPosition = OffBoard | OnBoard Position
data Position = Position deriving (Show, Eq, Ord)
