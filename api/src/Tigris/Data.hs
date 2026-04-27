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
    Piece (..),
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
    area
  )
where

import Control.Lens (makeLenses, ix, set)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid (Sum)
import Data.Semigroup (Min)
import Data.Array (Array, array)
import qualified Data.Map as Map

data Sphere = Temples | Markets | Settlements | Farms deriving (Show, Eq, Ord)
newtype Tile = Tile {_tileSphere :: Sphere} deriving (Show, Eq, Ord)
data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)
instance FromJSON Dynasty
instance ToJSON Dynasty


emptyBoard :: Board 
emptyBoard = Board {_numberOfTreasuresLeft = 10, _leaderPositions = Map.empty, _grid = emptyGrid, _regions = Map.empty}


emptyGrid :: Grid
emptyGrid = array ((1,1), (11,16)) $ do
  (rowIndex, row) <- zip [1..] rows
  (columnIndex, element) <- zip [1..] row
  return ((rowIndex, columnIndex), empty element) 
  where
    rows = 
      [ replicate 4 Sand <> replicate 5 River <> replicate 3 Sand <> [ River] <> replicate 3 Sand
      , replicate 4 Sand <> [River] <> replicate 7 Sand <> [River] <> replicate 3 Sand
      , replicate 3 Sand <> [River, River] <> replicate 7 Sand <> [River, River, Sand, Sand]
      , replicate 4 River <> replicate 9 Sand <> replicate 3 River
      , replicate 14 Sand <> [River, River]
      , replicate 14 Sand <> [River, Sand]
      , replicate 4 River <> replicate 8 Sand <> replicate 3 River <> [Sand]
      , replicate 3 Sand <> replicate 4 River <> replicate 5 Sand <> [River] <> replicate 3 Sand
      , replicate 6 Sand <> replicate 7 River <> replicate 3 Sand
      , replicate 16 Sand
      , replicate 16 Sand
      ]
    empty marking = Space {_marking = marking, _slot = Left (EmptySpace{_borderingRegions = Set.empty})}

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

data Space = Space {_marking :: Marking, _slot :: Either EmptySpace Piece} deriving Show

data EmptySpace = EmptySpace {_borderingRegions :: Set RegionKey} deriving Show
data Piece = LeaderPiece Leader | TilePiece Sphere deriving Show

type RegionKey = Min Space

data Marking = Sand | River deriving Show

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

startingBoard :: Board
startingBoard = putTemples emptyBoard where
  putTemples = applyAll putTemple temples where
    putTemple position = set (grid . ix position . slot) (Right (TilePiece Temples))
    temples = [(1,11), (2,2), (2,16), (3,6), (5,14), (7,9), (8,2), (9,15), (10,6), (11,11)]

applyAll :: (a -> b -> b) -> [a] -> b -> b
applyAll  = flip . foldr
