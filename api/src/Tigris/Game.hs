{-# LANGUAGE FlexibleContexts #-}
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
import Data.Map ((!))
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

zoomMaybeState :: ((s -> (Maybe a, s)) -> t -> (Maybe a, t)) -> MaybeT (State s) a -> MaybeT (State t) a
zoomMaybeState l = MaybeT . state . l . runState . runMaybeT

--  f (b, s)

liftStateT :: (Applicative f) => State s a -> StateT s f a
liftStateT = mapStateT (pure . runIdentity)

orFinish :: (Monad m) => Maybe a -> StateT Game m (Either Winners a)
orFinish = maybe (Left <$> determineWinners) (pure . Right)

increment :: (a -> Bool) -> a -> Sum Int
increment p = bool 0 1 . p

playInteraction :: (Monad m) => (Dynasty -> Interactions m) -> GameStage -> Game -> m (Either Winners GameStage, Game)
playInteraction interactions' (GameStage (currentPlayer : subsequentPlayers) turnNumber interaction) game = case interaction of
  RevoltDefence (RevoltDetails {revoltDefender, revoltArea, revoltDefenderPosition, revoltAttackerPosition}) revoltAttackValue -> runStateT (liftStateT . resolveRevolt =<< revoltDefence revoltDefenderPosition) game
    where
      revoltDefence position = (+) <$> zoom (board . grid) getAdjacentTemples <*> getCommittedTemples' revoltDefender
        where
          getAdjacentTemples = getAp . foldMap (Ap . countTemple) $ adjacentPositions position
          countTemple position' = uses (ix position' . slot . traverse) $ increment (== TilePiece Temples)
      resolveRevolt defenceValue = do
        bool
          (removeAttacker *> scoreOneTemple revoltDefender)
          (removeDefender *> scoreOneTemple revoltAttacker)
          attackerWins
        maybeGame <- runMaybeT continue'
        orFinish maybeGame
        where
          attackerWins = revoltAttackValue > defenceValue
      removeAttacker = removeFromBoard revoltAttackerPosition
      removeDefender = do
        removeFromBoard revoltDefenderPosition
        removeFromArea (KingdomKey revoltArea) revoltDefenderPosition
        joinInfluenceAt revoltAttackerPosition
      scoreOneTemple player = (players . at player . traverse . score) <>= Bag.one (SphereScore Temples)
      removeFromBoard position = (board . grid . ix position . slot) .= Nothing
  RevoltAttack details -> runStateT (Right . sameTurn . RevoltDefence details . undefined <$> getCommittedTemples' revoltAttacker) game
  Conflict _ -> undefined
  War conflicts -> (,game) . Right . sameTurn . Conflict <$> chooseConflict playerInteractions conflicts
  Turn -> flip runState game . (orFinish <=< applyAction) <$> getAction playerInteractions
  where
    revoltAttacker = currentPlayer
    getCommittedTemples' player = zoom (players . at player . traverse . hand . at Temples . traverse) (StateT f)
      where
        f (Sum playerTemples) = (Sum &&& Sum . (playerTemples -)) <$> getCommittedTemples (interactions' player) playerTemples
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
      replicateM_ (6 - handSize) (MaybeT dealOne')
    dealOne' = state . orPassThrough $ uncurry dealOne
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
          updateInfluence' areas' = lift $ joinInfluence (RegionKey (Min position)) areas' position
      (PositionLeader sphere maybePosition) -> case maybePosition of
        Just position -> maybe continue' (pure . sameTurn . RevoltAttack) =<< lift positionLeader
          where
            leader = Leader currentPlayer sphere
            positionLeader = runMaybeT . foldMapA (MaybeT . revoltOrAddInfluence) . getNeighbouringAreas =<< placeLeaderOnBoard currentPlayer sphere position
            revoltOrAddInfluence existingKey = maybe (Nothing <$ addLeaderToRegion position) (pure . Just) =<< maybeRevoltDetails existingKey
              where
                addLeaderToRegion = joinInfluence leaderKey $ Set.singleton existingKey
            leaderKey = KingdomKey (Set.singleton leader)
            maybeRevoltDetails (KingdomKey kingdomLeaders) = runMaybeT $ foldMapA fromLeader kingdomLeaders
              where
                fromLeader otherLeader = case otherLeader of
                  Leader otherPlayer otherSphere | otherSphere == sphere -> MaybeT . fmap (Just . revoltDetails) $ leaderPosition otherLeader
                    where
                      revoltDefender = otherPlayer
                      revoltArea = kingdomLeaders
                      leaderPosition leader' = uses (board . leaderPositions) (! leader') where
                      revoltAttackerPosition = position
                      revoltDetails revoltDefenderPosition = RevoltDetails {revoltArea, revoltDefender, revoltAttackerPosition, revoltDefenderPosition}
                  _ -> empty
            maybeRevoltDetails _ = pure Nothing
        Nothing -> lift (placeLeaderOffBoard currentPlayer sphere) *> continue'
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
