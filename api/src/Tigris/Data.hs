{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Tigris.Data
  ( Sphere (..),
    Tile (..),
    tileSphere,
    Dynasty (..),
    Leader (..),
    leaderSphere,
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
    Occupier (..),
    Marking (..),
    Bag,
    Hand,
    Score,
    Winners,
    Action (..),
    Position ,
    Interaction (..),
    ActionNumber (..),
    PlayingState(..),
    RevoltDetails(..),
    Region,
    Interactions (..),
    startingBoard,
    leaderDynasty,
    regions,
    slot,
    area,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Set (Set)
import Data.Monoid (Sum)
import Data.Semigroup (Min)
import Data.Array (Array, array)
import qualified Data.Map as Map

data Sphere = Temples | Markets | Settlements | Farms deriving (Show, Eq, Ord)
newtype Tile = Tile {_tileSphere :: Sphere} deriving (Show, Eq, Ord)
data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)
instance FromJSON Dynasty
instance ToJSON Dynasty

startingBoard :: Board
startingBoard = Board {_numberOfTreasuresLeft = 10, _leaderPositions = Map.empty, _grid = startingGrid, _regions = Map.empty}

startingGrid :: Grid
startingGrid = array ((1,1), (11,16)) []

data Leader = Leader {_leaderDynasty :: Dynasty, _leaderSphere :: Sphere} deriving (Show, Eq, Ord)

data PlayingState = PlayingState Interaction [Dynasty] Game deriving Show

data Game = Game {_bag :: [Tile], _players :: Map Dynasty PlayerInfo, _board :: Board} deriving (Show)

data ActionNumber = FirstAction | SecondAction  deriving (Show)
data Interaction = Turn ActionNumber | RevoltAttack RevoltDetails | RevoltDefence RevoltDetails deriving Show

data RevoltDetails = RevoltDetails { _revoltDefender :: Dynasty, _revoltSphere :: Sphere} deriving (Show)

type PlayerInfos = Map Dynasty PlayerInfo

data Board = Board {_numberOfTreasuresLeft :: Int, _leaderPositions :: LeaderPositions, _grid :: Grid, _regions :: Regions} deriving Show

type Regions = Map RegionKey Region

type Grid = Array Position Space

type LeaderPositions = Map Leader Position

data Region = Region { _area :: Set Space, _leaders :: Set Leader } deriving Show

data PlayerInfo = PlayerInfo {_score :: Score, _hand :: Bag Tile, _catastropheTiles :: Int} deriving Show

data Space = Space {_marking :: Marking, _slot :: Either EmptySpace OccupiedSpace} deriving Show

data EmptySpace = EmptySpace {_borderingRegions :: Set RegionKey} deriving Show
data OccupiedSpace = OccupiedSpace {_region :: RegionKey, _occupier :: Occupier} deriving Show
data Occupier = PlacedLeader Leader | PlacedTile Sphere deriving Show

type RegionKey = Min Space

data Marking = Sand | River | Temple deriving Show

type Bag a = Map a (Sum Int)

type Hand = Bag Tile

type Score = Map Sphere (Sum Int)

type Winners = [Dynasty]

data Action = PositionLeader Sphere (Maybe Position) | PlaceTile Sphere Position | PlayCatastrophe | ReplaceTiles Hand | Pass
type Position = (Int, Int)

makeLenses ''Tile
makeLenses ''Leader
makeLenses ''Game
makeLenses ''Board
makeLenses ''PlayerInfo
makeLenses ''Space
makeLenses ''Region

data Interactions m = Interactions {getAction :: m Action, getCommittedTemples :: m Int}
