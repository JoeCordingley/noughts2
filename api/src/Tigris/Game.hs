{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tigris.Game
  ( setupGame,
    playGame,
  )
where

import qualified Bag
import BasicPrelude hiding (empty, (<*), (\\))
import Control.Applicative (Alternative, empty)
import Control.Lens hiding (uncons)
import Control.Monad.Random.Lazy
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Foldable (fold)
import qualified Data.Map as Map
import Data.Monoid (Ap (..))
import Data.Semigroup
import qualified Data.Set as Set
import Lib (onFirst)
import System.Random.Shuffle (shuffleM)
import Tigris.Data

startingPlayerInfo :: Hand -> PlayerInfo
startingPlayerInfo startingHand = PlayerInfo {_score = startingScore, _hand = startingHand, _catastropheTiles = 2, _playerLeadersInHand = spheres}

one :: a -> Bag a
one a = Map.singleton a (Sum 1)

allCivilizationTiles :: Bag Sphere
allCivilizationTiles = Map.fromList [(Temples, 57), (Markets, 30), (Settlements, 30), (Farms, 36)]

tilesMinusStartingTemples :: Bag Sphere
tilesMinusStartingTemples = allCivilizationTiles <> Map.singleton Temples (-10)

bagToList :: Bag a -> [a]
bagToList = Map.foldrWithKey f []
  where
    f k (Sum n) ts = replicate n k <> ts

emptyHand :: Hand
emptyHand = allSpheresZero

setupGame :: (MonadRandom m) => Bimap Dynasty a -> m PlayingState
setupGame m = fromShuffled <$> shuffleM dynasties <*> shuffleM (bagToList tilesMinusStartingTemples)
  where
    fromShuffled dynasties' tiles = PlayingState (GameStage (cycle dynasties') FirstAction Turn) (Game {_bag = remainingTiles, _players = startingPlayerInfos, _board = startingBoard})
      where
        (startingPlayerInfos, remainingTiles) = runState (traverse (fmap startingPlayerInfo . dealUpToSix) emptyHands) tiles
        emptyHands = Map.fromList $ map (,emptyHand) dynasties'
    dynasties = Bimap.keys m

allSpheresZero :: Map Sphere (Sum Int)
allSpheresZero = Map.fromSet (const 0) spheres

startingScore :: Score
startingScore = Map.fromSet (const 0) scoreAreas

spheres :: Set Sphere
spheres = Set.fromList [Settlements, Temples, Farms, Markets]

scoreAreas :: Set ScoreArea
scoreAreas = Set.insert Treasure $ Set.map SphereScore spheres

playGame :: (Monad m) => (Dynasty -> Interactions m) -> (PlayingState -> m (Winners, Game)) -> PlayingState -> m (Winners, Game)
playGame interactions recurse (PlayingState gameStage game) = uncurry (either pure' recurse') =<< playInteraction interactions gameStage game
  where
    pure' winners' game' = pure (winners', game')
    recurse' gamestage' game' = recurse (PlayingState gamestage' game')

-- applyAction :: [Dynasty] -> ActionNumber -> Action -> State Game (Maybe GameStage)
-- applyAction (currentPlayer : subsequentPlayers) actionNumber action = runMaybeT $ case action of
--  Pass -> endTurn
--  (ReplaceTiles discards) -> replaceTiles *> continue
--    where
--      replaceTiles = zoomMaybeState (playersAndBag . onFirst (at currentPlayer . traverse . hand)) (lift removeDiscards *> dealHand)
--      removeDiscards = _1 %= (<> discards)
--  PlayCatastrophe -> undefined
--  (PlaceTile sphere position) -> do
--    NeighbouringAreas areas' <- lift (zoom board (placeTile sphere position))
--    maybe (updateInfluence' areas' *> continue) (pure . withSamePlayer . War) $ warDetails areas'
--    where
--      warDetails _ = Nothing
--      updateInfluence' areas' = lift $ updateInfluence (RegionKey (Min position)) areas' position
--  (PositionLeader sphere position) -> maybe continue (pure . withSamePlayer . RevoltAttack) =<< lift positionLeader
--    where
--      positionLeader = runMaybeT . foldMapA (MaybeT . revoltOrAddInfluence) . getNeighbouringAreas =<< placeLeader currentPlayer sphere position
--      keyAddition = KingdomKey (Set.singleton leader)
--      revoltOrAddInfluence :: AreaKey -> State Game (Maybe RevoltDetails)
--      revoltOrAddInfluence existingKey = maybe (Nothing <$ updateInfluence') (pure . Just) $ revoltDetails existingKey
--        where
--          updateInfluence' = traverse_ addLeaderToRegion position
--          addLeaderToRegion = updateInfluence keyAddition $ Set.singleton existingKey
--      leader = Leader currentPlayer sphere
--      revoltDetails (KingdomKey leaders') = foldMapA fromLeader leaders'
--        where
--          fromLeader (Leader opponent sphere') = if sphere == sphere' then Just $ RevoltDetails sphere leaders' opponent else Nothing
--      revoltDetails _ = Nothing
--  where
--    withSamePlayer = GameStage (currentPlayer : subsequentPlayers)
--    endTurn = do
--      treasuresLeft <- lift $ use (board . numberOfTreasuresLeft)
--      guard $ treasuresLeft > 2
--      refreshHands
--      return nextPlayer
--    nextPlayer = GameStage subsequentPlayers $ Turn FirstAction
--    refreshHands = zoomMaybeState (playersAndBag . onFirst (traverse . hand)) dealHand
--    dealHand = do
--      handSize <- lift $ uses _1 (getSum . Bag.size)
--      replicateM_ (6 - handSize) dealOne'
--    dealOne' = MaybeT . state . orPassThrough $ uncurry dealOne
--    continue = case actionNumber of
--      FirstAction -> pure . withSamePlayer $ Turn SecondAction
--      SecondAction -> endTurn
-- applyAction _ _ _ = undefined

zoomMaybeState :: ((s -> (Maybe a, s)) -> t -> (Maybe a, t)) -> MaybeT (State s) a -> MaybeT (State t) a
zoomMaybeState l = MaybeT . state . l . runState . runMaybeT

--  f (b, s)

liftStateT :: (Applicative f) => State s a -> StateT s f a
liftStateT = mapStateT (pure . runIdentity)

orFinish :: (Monad m) => Maybe a -> StateT Game m (Either Winners a)
orFinish = maybe (Left <$> determineWinners) (pure . Right)

revoltDefence :: (Monad m) => Position -> StateT Game m (Sum Int) -> StateT Game m (Sum Int)
revoltDefence position getCommittedTemples' = (+) <$> zoom (board . grid) getAdjacentTemples <*> getCommittedTemples'
  where
    getAdjacentTemples = getAp . foldMap (Ap . countTemple) $ adjacentPositions position
    countTemple position' = uses (ix position' . slot . traverse) $ count (== TilePiece Temples)

removeCommittedTemples :: Int -> StateT Game m ()
removeCommittedTemples = undefined

count :: (a -> Bool) -> a -> Sum Int
count p = bool 0 1 . p

playInteraction :: (Monad m) => (Dynasty -> Interactions m) -> GameStage -> Game -> m (Either Winners GameStage, Game)
playInteraction interactions' (GameStage (currentPlayer : subsequentPlayers) turnNumber interaction) game = case interaction of
  RevoltDefence (RevoltDefenceDetails {revoltDefender, revoltAttackValue, defenderPosition}) -> runStateT (liftStateT . resolveRevolt =<< revoltDefence defenderPosition (zoom (players . at revoltDefender . traverse . hand . at Temples . traverse) (StateT . revoltAttack $ interactions' revoltDefender))) game
    where
      resolveRevolt = orFinish <=< runMaybeT . const continue' <=< bool (removeAttacker *> scoreDefender) (removeDefender *> scoreAttacker) . attackerWins
      removeAttacker = undefined
      removeDefender = undefined
      scoreAttacker = undefined
      scoreDefender = undefined
      attackerWins = undefined
  RevoltAttack _ -> runStateT (undefined <$> zoom (players . at currentPlayer . traverse . hand . at Temples . traverse) (StateT $ revoltAttack playerInteractions)) game
  Conflict details -> undefined
  War conflicts -> (,game) . Right . GameStage (currentPlayer : subsequentPlayers) turnNumber . Conflict <$> chooseConflict playerInteractions conflicts
  Turn -> flip runState game . (orFinish <=< applyAction) <$> getAction playerInteractions
  where
    revoltAttack player (Sum playerTemples) = do
      committed <- getCommittedTemples player playerTemples
      return (Sum committed, Sum (playerTemples - committed))
    continue' = continue turnNumber
    sameTurn = withSamePlayer turnNumber
    withSamePlayer = GameStage (currentPlayer : subsequentPlayers)
    playerInteractions = interactions' currentPlayer
    continue FirstAction = pure $ withSamePlayer SecondAction Turn
    continue SecondAction = endTurn
    endTurn = do
      treasuresLeft <- lift $ use (board . numberOfTreasuresLeft)
      guard $ treasuresLeft > 2
      refreshHands
      return nextPlayer
    dealHand = do
      handSize <- lift $ uses _1 (getSum . Bag.size)
      replicateM_ (6 - handSize) dealOne'
    dealOne' = MaybeT . state . orPassThrough $ uncurry dealOne
    nextPlayer = GameStage subsequentPlayers FirstAction Turn
    refreshHands = zoomMaybeState (playersAndBag . onFirst (traverse . hand)) dealHand
    applyAction action = runMaybeT $ case action of
      Pass -> endTurn
      (ReplaceTiles discards) -> replaceTiles *> continue'
        where
          replaceTiles = zoomMaybeState (playersAndBag . onFirst (at currentPlayer . traverse . hand)) (lift removeDiscards *> dealHand)
          removeDiscards = _1 %= (<> discards)
      PlayCatastrophe -> undefined
      (PlaceTile sphere position) -> do
        NeighbouringAreas areas' <- lift (zoom board (placeTile sphere position))
        maybe (updateInfluence' areas' *> continue') (pure . sameTurn . War) $ warDetails areas'
        where
          warDetails _ = Nothing
          updateInfluence' areas' = lift $ updateInfluence (RegionKey (Min position)) areas' position
      (PositionLeader sphere position) -> maybe continue' (pure . sameTurn . RevoltAttack) =<< lift positionLeader
        where
          positionLeader = runMaybeT . foldMapA (MaybeT . revoltOrAddInfluence) . getNeighbouringAreas =<< placeLeader currentPlayer sphere position
          keyAddition = KingdomKey (Set.singleton leader)
          revoltOrAddInfluence :: AreaKey -> State Game (Maybe RevoltDetails)
          revoltOrAddInfluence existingKey = maybe (Nothing <$ updateInfluence') (pure . Just) $ revoltDetails existingKey
            where
              updateInfluence' = traverse_ addLeaderToRegion position
              addLeaderToRegion = updateInfluence keyAddition $ Set.singleton existingKey
          leader = Leader currentPlayer sphere
          revoltDetails (KingdomKey leaders') = foldMapA fromLeader leaders'
            where
              fromLeader (Leader opponent sphere') = if sphere == sphere' then Just $ RevoltDetails sphere leaders' opponent else Nothing
          revoltDetails _ = Nothing
      where

playInteraction _ _ _ = undefined

foldMapA :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapA f = foldr ((<|>) . f) empty

dealOne :: Hand -> [Tile] -> Maybe (Hand, [Tile])
dealOne hand' = fmap (first addToHand) . uncons
  where
    addToHand a = hand' <> Bag.one a

orPassThrough :: (a -> Maybe a) -> a -> (Maybe (), a)
orPassThrough f a = (void ma, fromMaybe a ma)
  where
    ma = f a

determineWinners :: (Monad f) => StateT Game f Winners
determineWinners = uses players (winners . fmap (view score))

winners :: Map Dynasty Score -> Winners
winners finalScores' = snd . Map.findMax $ Map.foldMapWithKey groupByScore finalScores'
  where
    -- TODO
    groupByScore k v = Map.singleton (addTreasures (Bag.lookup Treasure v) $ sort $ Map.elems v) [k]

addTreasures :: Sum Int -> [Sum Int] -> [Sum Int]
addTreasures 0 x = x
addTreasures n (x : xs) = addTreasures (n - 1) . sort $ (x + 1) : xs
addTreasures _ [] = undefined

dealUpToSix :: Hand -> State [Sphere] Hand
dealUpToSix playerTiles = foldr (>=>) pure (replicate (6 - total playerTiles) (state . dealOne')) playerTiles
  where
    dealOne' playerTiles' (x : xs) = (one x <> playerTiles', xs)
    dealOne' _ _ = undefined

total :: Bag a -> Int
total = getSum . fold

playersAndBag :: (Functor f) => ((PlayerInfos, [Tile]) -> f (PlayerInfos, [Tile])) -> Game -> f Game
playersAndBag f game' = (\(_players, _bag) -> game' {_players, _bag}) <$> f (_players game', _bag game')
