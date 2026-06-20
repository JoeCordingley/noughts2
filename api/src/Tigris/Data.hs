{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tigris.Data
  ( Sphere (..),
    Tile,
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
    influence,
    Influence (..),
    Grid,
    LeaderPositions,
    PlayerInfo (..),
    score,
    hand,
    catastropheTiles,
    adjacentPositions,
    Space (..),
    marking,
    Piece (..),
    Marking (..),
    Bag,
    Hand,
    Score,
    Winners,
    Action (..),
    Position,
    Interaction (..),
    ActionNumber (..),
    PlayingState (..),
    GameStage (..),
    RevoltDetails (..),
    Region,
    Interactions (..),
    EmptySpace (..),
    ScoreArea (..),
    AreaKey (..),
    NeighbouringAreas (..),
    areas,
    startingBoard,
    leaderDynasty,
    slot,
    area,
    sphereText,
    dynastyText,
    markingText,
    nextToTemples,
    playerLeadersInHand,
    leaderPositions,
    joinInfluence,
    placeLeaderOffBoard,
    placeLeaderOnBoard,
    region,
    surrounds,
    placeTile,
    removeFromArea,
    joinInfluenceAt,
  )
where

import Bag (Bag)
import BasicPrelude hiding (empty, (<*), (\\))
import Control.Lens hiding (uncons)
import Control.Monad.State
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Array (Array, array)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import Data.Monoid (Ap (..), Sum (..))
import Data.Semigroup (Min (..), Semigroup)
import Data.Set ((\\))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lib (tupled)

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
emptyBoard = Board {_numberOfTreasuresLeft = 10, _grid = emptyGrid, _leaderPositions = Map.empty, _influence = Map.empty}

emptyGrid :: Grid
emptyGrid = array ((1, 1), (11, 16)) $ do
  (rowIndex, row) <- zip [1 ..] rows
  (columnIndex, marking) <- zip [1 ..] row
  return ((rowIndex, columnIndex), empty marking)
  where
    rows =
      [ replicate 4 Sand <> replicate 5 River <> replicate 3 Sand <> [River] <> replicate 3 Sand,
        replicate 4 Sand <> [River] <> replicate 7 Sand <> [River] <> replicate 3 Sand,
        replicate 3 Sand <> [River, River] <> replicate 7 Sand <> [River, River, Sand, Sand],
        replicate 4 River <> replicate 9 Sand <> replicate 3 River,
        replicate 14 Sand <> [River, River],
        replicate 14 Sand <> [River, Sand],
        replicate 4 River <> replicate 8 Sand <> replicate 3 River <> [Sand],
        replicate 3 Sand <> replicate 4 River <> replicate 5 Sand <> [River] <> replicate 3 Sand,
        replicate 6 Sand <> replicate 7 River <> replicate 3 Sand,
        replicate 16 Sand,
        replicate 16 Sand
      ]
    empty marking = Space {_marking = marking, _slot = Nothing, _nextToTemples = False, _areas = Set.empty}

data Leader = Leader {_leaderDynasty :: Dynasty, _leaderSphere :: Sphere} deriving (Show, Eq, Ord)

data PlayingState = PlayingState GameStage Game

data GameStage = GameStage [Dynasty] ActionNumber Interaction

data Game = Game {_bag :: [Sphere], _players :: Map Dynasty PlayerInfo, _board :: Board}

data ActionNumber = FirstAction | SecondAction

data ConflictDetails = ConflictDetails deriving (Generic)

data Interaction = Turn | RevoltAttack RevoltDetails | RevoltDefence RevoltDetails RevoltAttackValue | War (NonEmpty ConflictDetails) | Conflict ConflictDetails

type RevoltAttackValue = Sum Int

data RevoltDetails = RevoltDetails {revoltArea :: Set Leader, revoltDefender :: Dynasty, revoltAttackerPosition :: Position, revoltDefenderPosition :: Position}

type PlayerInfos = Map Dynasty PlayerInfo

data Board = Board {_numberOfTreasuresLeft :: Int, _grid :: Grid, _leaderPositions :: Map Leader Position, _influence :: Map AreaKey Influence}

data Influence = Influence {_region :: Set Position, _surrounds :: Set Position} deriving (Show)

instance Semigroup Influence where
  (Influence lr ls) <> (Influence rr rs) = Influence (lr <> rr) (ls <> rs)

type Grid = Array Position Space

type LeaderPositions = Map Leader Position

data Region = Region {_area :: Set Space, _leaders :: Set Leader}

data PlayerInfo = PlayerInfo {_score :: Score, _hand :: Bag Sphere, _catastropheTiles :: Int, _playerLeadersInHand :: Set Sphere}

data Space = Space {_marking :: Marking, _slot :: Maybe Piece, _nextToTemples :: Bool, _areas :: Set AreaKey}

data AreaKey = RegionKey (Min Position) | KingdomKey (Set Leader) deriving (Show, Eq, Ord)

instance Semigroup AreaKey where
  RegionKey x <> RegionKey y = RegionKey $ x <> y
  KingdomKey x <> KingdomKey y = KingdomKey $ x <> y
  KingdomKey x <> RegionKey _ = KingdomKey x
  RegionKey _ <> KingdomKey y = KingdomKey y

newtype EmptySpace = EmptySpace {_borderingRegions :: Set RegionKey}

data Piece = LeaderPiece Dynasty Sphere | TilePiece Sphere deriving (Eq)

type RegionKey = Min Space

data Marking = Sand | River

type Hand = Bag Sphere

type Score = Bag ScoreArea

data ScoreArea = SphereScore Sphere | Treasure deriving (Eq, Ord)

type Winners = [Dynasty]

data Action = PositionLeader Sphere (Maybe Position) | PlaceTile Sphere Position | PlayCatastrophe | ReplaceTiles Hand | Pass deriving (Generic)

type Position = (Int, Int)

makeLenses ''Leader
makeLenses ''Game
makeLenses ''Board
makeLenses ''PlayerInfo
makeLenses ''Space
makeLenses ''Region
makeLenses ''Influence

data Interactions m = Interactions {getAction :: m Action, getCommittedTemples :: Int -> m Int, chooseConflict :: NonEmpty ConflictDetails -> m ConflictDetails}

startingBoard :: Board
startingBoard = execState (traverse_ putTemple temples) emptyBoard
  where
    temples = [(1, 11), (2, 2), (2, 16), (3, 6), (5, 14), (7, 9), (8, 2), (9, 15), (10, 6), (11, 11)]

putTemple :: Position -> State Board ()
putTemple position = placeTile Temples position *> addInfluence key' position *> traverse_ setTempleAdjacent (adjacentPositions position)
  where
    key' = RegionKey $ Min position

setTempleAdjacent :: Position -> State Board ()
setTempleAdjacent adjacency = (grid . ix adjacency . nextToTemples) .= True

-- putTemple position = over grid $ applyAll setTempleAdjacent (adjacentPositions position) . set (ix position . slot) (Just (TilePiece Temples))
--  where
--    setTempleAdjacent adjacency = set (ix adjacency . nextToTemples) True

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

joinInfluence :: AreaKey -> Set AreaKey -> Position -> State Game ()
joinInfluence key toKeys position = do
  positions <- removeInfluence removedKeys
  traverse_ (zoom board . addInfluence resultingKey) (Set.insert position positions)
  where
    resultingKey = foldr (<>) key toKeys
    removedKeys = Set.filter (/= resultingKey) toKeys

removeFromArea :: AreaKey -> Position -> State Game ()
removeFromArea area' removedPosition = do
  positions <- removeInfluence $ Set.singleton area'
  traverse_ joinInfluenceAt (Set.delete removedPosition positions)

joinInfluenceAt :: Position -> StateT Game Identity ()
joinInfluenceAt position = traverse_ joinInfluence' =<< uses (board . grid . ix position) (tupled . (fmap areaKey . view slot &&& Just . view areas))
  where
    joinInfluence' (key, toKeys) = joinInfluence key toKeys position
    areaKey (LeaderPiece dynasty sphere) = KingdomKey . Set.singleton $ Leader dynasty sphere
    areaKey (TilePiece _) = RegionKey $ Min position

addInfluence :: AreaKey -> Position -> State Board ()
addInfluence areaKey position = ((influence . at areaKey) <>= influence') *> traverse_ addToGrid adjacentPositions'
  where
    influence' = Just $ Influence {_region = Set.singleton position, _surrounds = adjacentPositions'}
    addToGrid :: Position -> State Board ()
    addToGrid position' = grid . ix position' . areas %= Set.insert areaKey
    adjacentPositions' :: Set Position
    adjacentPositions' = Set.insert position $ Set.fromList $ adjacentPositions position

removeInfluence :: Set AreaKey -> State Game (Set Position)
removeInfluence fromKeys = do
  (region', surrounds') <- getAp (foldMap (Ap . removeInfluenceFromMap) fromKeys)
  traverse_ removeInfluenceFromBoardAt surrounds'
  return region'
  where
    removeInfluenceFromBoardAt :: Position -> State Game ()
    removeInfluenceFromBoardAt position = board . grid . ix position . areas %= (\\ fromKeys)
    removeInfluenceFromMap :: AreaKey -> State Game (Set Position, Set Position)
    removeInfluenceFromMap areaKey = board . influence . at areaKey %%= ((,Nothing) . foldMap (view region &&& view surrounds))

placeLeaderOnBoard :: Dynasty -> Sphere -> Position -> State Game NeighbouringAreas
placeLeaderOnBoard dynasty sphere position = swapOutLeaderPosition dynasty sphere (Just position) *> addToBoard
  where
    addToBoard = fmap NeighbouringAreas (board . grid . ix position %%= (view areas &&& set slot (Just leader)))
    leader = LeaderPiece dynasty sphere

swapOutLeaderPosition :: Dynasty -> Sphere -> Maybe Position -> State Game ()
swapOutLeaderPosition dynasty sphere maybePosition = maybe removeFromHand removeFromBoard =<< board . leaderPositions . at (Leader dynasty sphere) %%= (,maybePosition)
  where
    removeFromHand = playerLeadersInHand' %= Set.delete sphere
    removeFromBoard position = slot' position .= Nothing
    playerLeadersInHand' = players . at dynasty . traverse . playerLeadersInHand
    slot' position = board . grid . ix position . slot

placeLeaderOffBoard :: Dynasty -> Sphere -> State Game ()
placeLeaderOffBoard dynasty sphere = swapOutLeaderPosition dynasty sphere Nothing *> addToHand
  where
    addToHand = playerLeadersInHand' %= Set.insert sphere
    playerLeadersInHand' = players . at dynasty . traverse . playerLeadersInHand

newtype NeighbouringAreas = NeighbouringAreas {getNeighbouringAreas :: Set AreaKey}

placeTile :: Sphere -> Position -> State Board NeighbouringAreas
placeTile sphere position = fmap NeighbouringAreas $ (grid . ix position) %%= (view areas &&& (set slot . Just $ TilePiece sphere))

newtype CommittedTemples = CommittedTemples {value :: Int} deriving (Generic)

instance ToJSON CommittedTemples

instance FromJSON CommittedTemples

instance ToJSON Action

instance FromJSON Action

instance FromJSON ConflictDetails
