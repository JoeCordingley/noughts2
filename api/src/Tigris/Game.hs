{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Tigris.Game
  (
    setupGame,
    playGame,
  )
where

import Control.Lens
import Control.Monad.Random.Lazy
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random.Shuffle (shuffleM)
import Data.List (sort)
import Control.Monad.State (StateT(..), runStateT)
import Data.Monoid (Sum(..))
import Data.Maybe (fromJust)
import Tigris.Data
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Foldable (fold)



startingPlayerInfo :: Hand -> PlayerInfo
startingPlayerInfo startingHand = PlayerInfo{_score = startingScore, _hand = startingHand, _catastropheTiles = 2, _playerLeadersInHand = spheres}

one :: a -> Bag a 
one a = Map.singleton a (Sum 1)


allCivilizationTiles :: Bag Sphere
allCivilizationTiles = Map.fromList $ [(Temples, 57), (Markets, 30), (Settlements, 30), (Farms,  36)]

tilesMinusStartingTemples :: Bag Sphere
tilesMinusStartingTemples = allCivilizationTiles <> Map.singleton Temples (-10)

bagToList :: Bag a -> [a]
bagToList = Map.foldrWithKey f [] where
  f k (Sum n) ts = replicate n k <> ts

emptyHand :: Hand
emptyHand = allSpheresZero

setupGame :: (MonadRandom m) => Bimap Dynasty a -> m PlayingState
setupGame m = fromShuffled <$> shuffleM dynasties <*> shuffleM (bagToList tilesMinusStartingTemples)
  where
    fromShuffled dynasties' tiles = PlayingState (Turn FirstAction) (cycle dynasties') (Game {_bag = remainingTiles, _players = fmap startingPlayerInfo startingPlayerHand, _board = startingBoard}) where
      (startingPlayerHand, remainingTiles) = fromJust $ state traverse dealUpToSix (emptyHands, tiles)
      emptyHands = Map.fromList $ map (,emptyHand) dynasties'
    dynasties = Bimap.keys m

winners :: Map Dynasty Score -> Winners
winners finalScores' = snd . Map.findMax $ Map.foldMapWithKey groupByScore finalScores' where
  groupByScore k v = Map.singleton (sort $ Map.elems v) [k]


allSpheresZero :: Map Sphere (Sum Int)
allSpheresZero = Map.fromSet (const 0) spheres

startingScore :: Score
startingScore = Map.fromSet (const 0) scoreAreas

spheres :: Set Sphere
spheres = Set.fromList [Settlements, Temples, Farms, Markets]

scoreAreas :: Set ScoreArea
scoreAreas = Set.insert Treasure $ Set.map SphereScore spheres

playGame :: Monad m => (Dynasty -> Interactions m) -> (PlayingState -> m Winners) -> PlayingState -> m Winners
playGame interactions recurse (PlayingState turn playerOrder game) = either pure recurse =<< playInteraction interactions turn playerOrder game

playInteraction :: Monad m => (Dynasty -> Interactions m) -> Interaction -> [Dynasty] -> Game -> m (Either Winners PlayingState)
playInteraction interactions' interaction (currentPlayer:subsequentPlayers) game = case interaction of
  Turn turnNumber -> applyAction <$> getAction playerInteractions where
    applyAction Pass = endTurn game
    applyAction (ReplaceTiles discards) = continue =<< maybe (Left $ determineWinners game) Right ((playerAndBag . state (at currentPlayer . traverse . hand)) (dealUpToSix . (<>) discards) game)
    applyAction PlayCatastrophe = undefined
    applyAction (PlaceTile _ _) = undefined
    applyAction (PositionLeader sphere leaderPosition) = trace "here" (continue . placeLeader currentPlayer sphere leaderPosition) game 
    --        revolt dynasty = Right . PlayingState (RevoltAttack (RevoltDetails {_revoltDefender = dynasty, _revoltSphere = sphere})) (currentPlayer:subsequentPlayers)
    nextPlayer = PlayingState (Turn FirstAction) subsequentPlayers 
    endTurn game' = maybe (Left $ determineWinners game') (Right . nextPlayer) (guard continueIfEnoughTreasures *> refreshHands game) where
      continueIfEnoughTreasures = view (board . numberOfTreasuresLeft) game' > 2
    continue = case turnNumber of
      FirstAction -> Right . PlayingState (Turn SecondAction) (currentPlayer:subsequentPlayers)
      SecondAction -> endTurn
    refreshHands = playerAndBag $ state (traverse . hand) dealUpToSix
  RevoltAttack _ -> undefined
  RevoltDefence _ -> undefined
  where
    playerInteractions = interactions' currentPlayer
playInteraction _ _ [] _ = undefined

--addLeaderToRegion :: Leader -> Region -> Game -> Game
--addLeaderToRegion = undefined

placeLeader :: Dynasty -> Sphere -> Maybe Position -> Game -> Game
placeLeader dynasty sphere maybePosition = maybe addToHand addToBoard maybePosition . uncurry (maybe removeFromHand removeFromBoard) . swapOutLeaderPosition where
  swapOutLeaderPosition = (board . leaderPositions . at (Leader dynasty sphere)) (, maybePosition)
  addToBoard position = set (slot' position) (Just $ LeaderPiece dynasty sphere) 
  addToHand = over playerLeadersInHand' $ Set.insert sphere 
  removeFromBoard position = set (slot' position) Nothing
  removeFromHand = over playerLeadersInHand' $ Set.delete sphere
  playerLeadersInHand' = players . at dynasty . traverse . playerLeadersInHand
  slot' position = board . grid . ix position . slot



--playTurn :: (Monad f) => (Dynasty -> Game -> f (Either Game (Either Winners Game))) -> Dynasty -> Game -> f (Either Winners Game)
--playTurn getTurn player = runExceptT . (liftEither . (maybeEndGame <=< endTurn) <=< playUpToTwoTurns) where
--  playUpToTwoTurns = orReturnAfterPass . twice turn
--  orReturnAfterPass = ExceptT . fmap (either Right id) . runExceptT . runExceptT
--  turn = ExceptT . ExceptT . getTurn player


determineWinners :: Game -> Winners 
determineWinners = winners . fmap (view score) . view players 

--orDetermineWinners :: (Game -> Maybe Game) -> Game -> Either Winners Game
--orDetermineWinners f game' = maybe (Left . winners . fmap (view score) $ view players game') Right $ f game'

--maybeEndGame :: Game -> Either Winners Game
--maybeEndGame = orDetermineWinners continueGame



--getTurn :: Functor f => (Dynasty -> Game -> f Action) -> Dynasty -> Game -> f (Either Game (Either Winners Game))
--getTurn getAction dynasty game' = flip applyAction game' <$> getAction dynasty game' where
--  applyAction Pass = Left 
--  applyAction (ReplaceTiles discards) = Right . (orDetermineWinners . playerAndBag $ state (at dynasty . traverse . hand) $ dealUpToSix . (<>) discards)
--  applyAction (PositionLeader sphere leaderPosition) = Right . resolveRevolts . (over board $ placeLeader (Leader dynasty sphere) leaderPosition) where
--    resolveRevolts :: Game -> Either Winners Game
--    resolveRevolts game = case leaderPosition of
--      Nothing -> Right game
--      Just position -> case opposingLeader' $ view (board . grid ) game of
--        Just opposingDynasty -> resolveRevolt opposingDynasty
--        Nothing -> Right game
--      where
--        opposingLeader' grid = leaderPosition >>= flip (opposingLeader sphere) grid
--  applyAction (PlaceTile sphere position) = Right . Right . (set (board . grid . at position . traverse . placedPiece) . Just $ PlacedTile sphere)


--opposingLeader :: Sphere -> Position -> Grid -> Maybe Dynasty
--opposingLeader sphere position = firstJust matchingLeader . view (at position . traverse . leaders)

--firstJust :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
--firstJust f = foldr ((<|>) . f) Nothing

--endTurn :: Game -> Either Winners Game
--endTurn = orDetermineWinners . playerAndBag $ state (traverse . hand) dealUpToSix

state :: ((a -> StateT st f b) -> s -> StateT st f t) -> (a -> StateT st f b) -> (s, st) -> f (t, st)
state l f (s, st) = runStateT (l f s) st

dealUpToSix :: Hand -> StateT [Sphere] Maybe Hand
dealUpToSix playerTiles = foldr (>=>) pure (replicate (6 - total playerTiles) (StateT . dealOne)) playerTiles where
  dealOne playerTiles' (x:xs) = Just (one x <> playerTiles', xs)
  dealOne _ [] = Nothing

total :: Bag a -> Int
total = getSum . fold


playerAndBag :: Functor f => ((PlayerInfos, [Tile]) -> f (PlayerInfos, [Tile])) -> Game -> f Game
playerAndBag f game' = (\(_players, _bag) -> game'{_players, _bag}) <$> f (_players game', _bag game')
