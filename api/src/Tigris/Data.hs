{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Tigris.Data
  ( Sphere (..),
    Tile ,
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
    EmptySpace(..),
    ScoreArea(..),
    startingBoard,
    leaderDynasty,
    slot,
    area,
    sphereText,
    dynastyText,
    markingText,
    nextToTemples,
    playerLeadersInHand,
    leaderPositions
  )
where

import Control.Lens (makeLenses, ix, set, over)
import Data.Aeson (FromJSON, ToJSON, ToJSONKey, FromJSONKey)
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Monoid (Sum)
import Data.Semigroup (Min)
import Data.Array (Array, array)
import Data.Text (Text)

data Sphere = Temples | Markets | Settlements | Farms deriving (Show, Eq, Ord, Generic)

type Tile = Sphere 
instance ToJSON Sphere
instance ToJSONKey Sphere
instance FromJSON Sphere
instance FromJSONKey Sphere
data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)
instance FromJSON Dynasty
instance ToJSON Dynasty


emptyBoard :: Board 
emptyBoard = Board {_numberOfTreasuresLeft = 10,  _grid = emptyGrid, _leaderPositions = Map.empty}


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
    empty marking = Space {_marking = marking, _slot = Nothing, _nextToTemples = False}

data Leader = Leader {_leaderDynasty :: Dynasty, _leaderSphere :: Sphere} deriving (Show, Eq, Ord)

data PlayingState = PlayingState Interaction [Dynasty] Game deriving Show

data Game = Game {_bag :: [Sphere], _players :: Map Dynasty PlayerInfo, _board :: Board} deriving (Show)

data ActionNumber = FirstAction | SecondAction  deriving (Show)
data Interaction = Turn ActionNumber | RevoltAttack RevoltDetails | RevoltDefence RevoltDetails deriving Show

data RevoltDetails = RevoltDetails { _revoltDefender :: Dynasty, _revoltSphere :: Sphere} deriving (Show)

type PlayerInfos = Map Dynasty PlayerInfo

data Board = Board {_numberOfTreasuresLeft :: Int,  _grid :: Grid, _leaderPositions :: Map Leader Position} deriving Show

type Grid = Array Position Space

type LeaderPositions = Map Leader Position

data Region = Region { _area :: Set Space, _leaders :: Set Leader } deriving Show

data PlayerInfo = PlayerInfo {_score :: Score, _hand :: Bag Sphere, _catastropheTiles :: Int, _playerLeadersInHand :: Set Sphere} deriving Show

data Space = Space {_marking :: Marking, _slot :: Maybe Piece, _nextToTemples :: Bool} deriving Show

data EmptySpace = EmptySpace {_borderingRegions :: Set RegionKey} deriving Show
data Piece = LeaderPiece Dynasty Sphere | TilePiece Sphere deriving Show

type RegionKey = Min Space

data Marking = Sand | River deriving Show

type Bag a = Map a (Sum Int)

type Hand = Bag Sphere

type Score = Map ScoreArea (Sum Int)

data ScoreArea = SphereScore Sphere | Treasure deriving (Show, Eq, Ord)

type Winners = [Dynasty]

data Action = PositionLeader Sphere (Maybe Position) | PlaceTile Sphere Position | PlayCatastrophe | ReplaceTiles Hand | Pass deriving (Generic)

type Position = (Int, Int)

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
    temples = [(1,11), (2,2), (2,16), (3,6), (5,14), (7,9), (8,2), (9,15), (10,6), (11,11)]

putTemple :: Position -> Board -> Board
putTemple position = over grid $ applyAll setTempleAdjacent (adjacentPositions position) . set (ix position . slot) (Just (TilePiece Temples)) where
  setTempleAdjacent adjacency = set (ix adjacency . nextToTemples) True

adjacentPositions :: Position -> [Position]
adjacentPositions (i, j) = ((i,) <$> adjacentColumns j) <> ((,j) <$> adjacentRows i) 

adjacentColumns :: Int -> [Int]
adjacentColumns 1 = [2]
adjacentColumns 16 = [15]
adjacentColumns j = [j - 1, j + 1]

adjacentRows :: Int -> [Int]
adjacentRows 1 = [2]
adjacentRows 11 = [10]
adjacentRows i = [i - 1, i + 1]


applyAll :: (a -> b -> b) -> [a] -> b -> b
applyAll  = flip . foldr

sphereText :: Sphere -> Text
sphereText Temples = "temples"
sphereText Markets = "markets"
sphereText Settlements = "settlements"
sphereText Farms = "farms"

dynastyText :: Dynasty -> Text
dynastyText Archer = "archer"
dynastyText Bull = "bull"
dynastyText Pot = "pot"
dynastyText Lion = "lion"

markingText :: Marking -> Text
markingText Sand = "sand"
markingText River = "river"

data CommittedTemples = CommittedTemples { value :: Int} deriving Generic
instance ToJSON CommittedTemples
instance FromJSON CommittedTemples
instance ToJSON Action
instance FromJSON Action


