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
import Control.Monad.Except
import Control.Monad.Random.Lazy
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Foldable (fold)
import qualified Data.Map as Map
import Data.Monoid (Ap (..))
import Data.Semigroup
import Data.Set ((\\))
import qualified Data.Set as Set
import Lib (onFirst)
import System.Random.Shuffle (shuffleM)
import Tigris.Data

(<<) :: (Applicative f) => f a -> f b -> f a
fa << fb = fb *> fa

startingPlayerInfo :: Hand -> PlayerInfo
startingPlayerInfo startingHand = PlayerInfo {_score = startingScore, _hand = startingHand, _catastropheTiles = 2, _playerLeadersInHand = spheres}

one :: a -> Bag a
one a = Map.singleton a (Sum 1)

allCivilizationTiles :: Bag Sphere
allCivilizationTiles = Map.fromList $ [(Temples, 57), (Markets, 30), (Settlements, 30), (Farms, 36)]

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
    fromShuffled dynasties' tiles = PlayingState (GameStage (cycle dynasties') (Turn FirstAction)) (Game {_bag = remainingTiles, _players = startingPlayerInfos, _board = startingBoard})
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
playGame interactions recurse (PlayingState gameStage game) = uncurry (either pure' recurse') =<< runStateT (playInteraction interactions gameStage) game
  where
    pure' winners' game' = pure (winners', game')
    recurse' gamestage' game' = recurse (PlayingState gamestage' game')

applyAction :: [Dynasty] -> ActionNumber -> Action -> State Game (Maybe GameStage)
applyAction (currentPlayer : subsequentPlayers) actionNumber action = runMaybeT $ case action of
  Pass -> endTurn
  (ReplaceTiles discards) -> replaceTiles *> continue
    where
      replaceTiles = zoomMaybe (playersAndBag . onFirst (at currentPlayer . traverse . hand)) (lift removeDiscards *> dealHand)
      removeDiscards = _1 %= (<> discards)
  PlayCatastrophe -> undefined
  (PlaceTile _ _) -> undefined
  (PositionLeader sphere position) -> maybe continue (pure . continueWithRevolt) =<< lift positionLeader
    where
      positionLeader = runMaybeT . foldMapA (MaybeT . revoltOrAddInfluence) =<< placeLeader currentPlayer sphere position
      revoltOrAddInfluence :: AreaKey -> State Game (Maybe RevoltDetails)
      revoltOrAddInfluence areaKey = maybe (state ((Nothing,) . updateInfluence')) (pure . Just) $ revoltDetails areaKey
        where
          updateInfluence' = (flip . foldr) (addLeaderToRegion (Leader currentPlayer sphere) areaKey) position
      revoltDetails (KingdomKey leaders') = foldMapA fromLeader leaders'
        where
          fromLeader (Leader opponent sphere') = if sphere == sphere' then Just RevoltDetails {_revoltSphere = sphere, _revoltDefender = opponent, _revoltArea = leaders'} else Nothing
      revoltDetails _ = Nothing
      continueWithRevolt = GameStage (currentPlayer : subsequentPlayers) . RevoltAttack
  where
    endTurn = do
      treasuresLeft <- lift $ use (board . numberOfTreasuresLeft)
      guard $ treasuresLeft > 2
      refreshHands
      return nextPlayer
    nextPlayer = GameStage subsequentPlayers $ Turn FirstAction
    refreshHands = zoomMaybe (playersAndBag . onFirst (traverse . hand)) dealHand
    dealHand = do
      handSize <- lift $ gets (Bag.size . fst)
      replicateM_ (6 - handSize) dealOne'
    dealOne' = MaybeT . state . orPassThrough $ uncurry dealOne
    continue = case actionNumber of
      FirstAction -> pure . GameStage (currentPlayer : subsequentPlayers) $ Turn SecondAction
      SecondAction -> endTurn
applyAction _ _ _ = undefined

zoomMaybe :: ((s -> (Maybe a, s)) -> t -> (Maybe a, t)) -> MaybeT (State s) a -> MaybeT (State t) a
zoomMaybe l = MaybeT . state . l . runState . runMaybeT

-- zoomMaybe l = (mapMaybeT . mapState) f where
--  f (b, s)

-- applyAction :: [Dynasty] -> ActionNumber -> Action -> ES Winners Game GameStage
-- applyAction (currentPlayer : subsequentPlayers) actionNumber action = case action of
--  Pass -> endTurn
--  (ReplaceTiles discards) -> continue << maybeDetermineWinners replaceTiles
--    where
--      replaceTiles = (playersAndBag . onFirst (at currentPlayer . traverse . hand)) (dealHand . first (<> discards))
--  PlayCatastrophe -> undefined
--  (PlaceTile _ _) -> undefined
--  (PositionLeader sphere position) -> maybe continue (pure . GameStage (currentPlayer : subsequentPlayers) . RevoltAttack) =<< EST (lift positionLeader)
--    where
--      positionLeader = runMaybeT . foldMapA (MaybeT . revoltOrAddInfluence) =<< placeLeader currentPlayer sphere position
--      revoltOrAddInfluence :: AreaKey -> State Game (Maybe RevoltDetails)
--      revoltOrAddInfluence areaKey = maybe (state ((Nothing,) . updateInfluence')) (pure . Just) $ revoltDetails areaKey
--        where
--          updateInfluence' = (flip . foldr) (addLeaderToRegion (Leader currentPlayer sphere) areaKey) position
--      revoltDetails (KingdomKey leaders') = foldMapA fromLeader leaders'
--        where
--          fromLeader (Leader opponent sphere') = if sphere == sphere' then Just RevoltDetails {_revoltSphere = sphere, _revoltDefender = opponent, _revoltArea = leaders'} else Nothing
--      revoltDetails _ = Nothing
--  where
--    endTurn = nextPlayer <$ refreshHands << finishDueToTreasures
--    finishDueToTreasures = fromStateT (inWhichCaseDetermineWinners =<< uses (board . numberOfTreasuresLeft) (<= 2))
--    inWhichCaseDetermineWinners = bool (pure $ Right ()) (Left <$> determineWinners)
--    refreshHands = maybeDetermineWinners dealHands
--    maybeDetermineWinners f = fromStateT (inWhichCaseDetermineWinners =<< state (orPassThrough f))
--    nextPlayer = GameStage subsequentPlayers $ Turn FirstAction
--    dealHands = (playersAndBag . onFirst (traverse . hand)) dealHand
--    dealHand (hand', bag') = nest (6 - Bag.size hand') (uncurry dealOne) (hand', bag')
--    continue = case actionNumber of
--      SecondAction -> endTurn
--      FirstAction -> pure . GameStage (currentPlayer : subsequentPlayers) $ Turn SecondAction
-- applyAction _ _ _ = undefined

newtype EST e s m a = EST {toExceptT :: ExceptT e (StateT s m) a}

type ES e s a = EST e s Identity a

fromStateT :: StateT s m (Either e a) -> EST e s m a
fromStateT = EST . ExceptT

instance (Functor f) => Functor (EST e s f) where
  fmap f (EST es) = EST (fmap f es)

instance (Monad m) => Applicative (EST e s m) where
  pure = EST . pure
  (EST l) <*> (EST r) = EST (l <*> r)

instance (Monad m) => Monad (EST e s m) where
  (EST fa) >>= f = EST (fa >>= toExceptT . f)

instance MonadTrans (EST e s) where
  lift = EST . lift . lift

runES :: EST e s m a -> s -> m (Either e a, s)
runES (EST es) s = runStateT (runExceptT es) s

mapES :: (m (Either e a, s) -> m' (Either e' a', s)) -> EST e s m a -> EST e' s m' a'
mapES f (EST es) = EST $ (mapExceptT . mapStateT) f es

playInteraction :: (Monad m) => (Dynasty -> Interactions m) -> GameStage -> StateT Game m (Either Winners GameStage)
playInteraction interactions' (GameStage (currentPlayer : subsequentPlayers) interaction) = case interaction of
  RevoltDefence _ -> undefined
  RevoltAttack _ -> undefined
  Turn turnNumber -> mapStateT coerce . (maybe (Left <$> determineWinners) (pure . Right) <=< applyAction (currentPlayer : subsequentPlayers) turnNumber) =<< lift (getAction playerInteractions)
    where
      playerInteractions = interactions' currentPlayer
playInteraction _ _ = undefined

coerce :: (Applicative f) => Identity a -> f a
coerce = pure . runIdentity

nest :: (Monad m) => Int -> (a -> m a) -> a -> m a
nest n = foldr (>=>) return . replicate n

foldMapA :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapA f = foldr ((<|>) . f) empty

dealOne :: Hand -> [Tile] -> Maybe (Hand, [Tile])
dealOne hand' = fmap (first addToHand) . uncons
  where
    addToHand a = (hand' <> Bag.one a)

orPassThrough :: (a -> Maybe a) -> a -> (Maybe (), a)
orPassThrough f a = (void ma, fromMaybe a ma)
  where
    ma = f a

-- orFinish :: (a -> Maybe a) -> a -> (IsFinished, a)
-- orFinish f a = maybe (IsFinished True, a) (IsFinished False,) $ f a

addLeaderToRegion :: Leader -> AreaKey -> Position -> Game -> Game
addLeaderToRegion leader = updateInfluence (KingdomKey (Set.singleton leader)) . Set.singleton

updateInfluence :: AreaKey -> Set AreaKey -> Position -> Game -> Game
updateInfluence key toKeys position = addInfluence resultingKey position . transferInfluence resultingKey removedKeys
  where
    resultingKey = foldr (<>) key toKeys
    removedKeys = Set.filter (/= resultingKey) toKeys

addInfluence :: AreaKey -> Position -> Game -> Game
addInfluence areaKey position = over (board . influence . at areaKey . traverse) addInfluence'
  where
    addInfluence' (Influence {_region, _surrounds}) = Influence {_region = Set.insert position _region, _surrounds = (Set.difference (Set.fromList $ adjacentPositions') _region) <> _surrounds} where
    adjacentPositions' = adjacentPositions position

transferInfluence :: AreaKey -> Set AreaKey -> Game -> Game
transferInfluence newKey fromKeys = (uncurry . flip . foldr) transferPosition . runState (getAp (foldMap (Ap . transferMap) fromKeys))
  where
    transferPosition position = over (board . grid . ix position . areas) (Set.insert newKey . (\\ fromKeys))
    transferMap :: AreaKey -> State Game (Set Position)
    transferMap areaKey = (board . influence . at areaKey) %%= ((,Nothing) . foldMap bothAreas)
    bothAreas (Influence {_region, _surrounds}) = _region <> _surrounds

placeLeader :: Dynasty -> Sphere -> Maybe Position -> State Game (Set AreaKey)
placeLeader dynasty sphere maybePosition = const addToNewPosition =<< modify . maybe removeFromHand removeFromBoard =<< swapOutLeaderPosition
  where
    addToNewPosition = maybe (state $ (Set.empty,) . addToHand) addToBoard maybePosition
    addToHand = over playerLeadersInHand' (Set.insert sphere)
    addToBoard :: Position -> State Game (Set AreaKey)
    addToBoard position = zoom (position' position) (const (use areas) =<< slot .= Just (LeaderPiece dynasty sphere))
    removeFromHand = over playerLeadersInHand' (Set.delete sphere)
    removeFromBoard position = set (slot' position) Nothing
    swapOutLeaderPosition = board . leaderPositions . at (Leader dynasty sphere) %%= (,maybePosition)
    playerLeadersInHand' = players . at dynasty . traverse . playerLeadersInHand
    slot' position = position' position . slot
    position' position = board . grid . ix position

determineWinners :: State Game Winners
determineWinners = uses players (winners . fmap (view score))

winners :: Map Dynasty Score -> Winners
winners finalScores' = snd . Map.findMax $ Map.foldMapWithKey groupByScore finalScores'
  where
    groupByScore k v = Map.singleton (sort $ Map.elems v) [k]

dealUpToSix :: Hand -> State [Sphere] Hand
dealUpToSix playerTiles = foldr (>=>) pure (replicate (6 - total playerTiles) (state . dealOne')) playerTiles
  where
    dealOne' playerTiles' (x : xs) = (one x <> playerTiles', xs)
    dealOne' _ _ = undefined

total :: Bag a -> Int
total = getSum . fold

playersAndBag :: (Functor f) => ((PlayerInfos, [Tile]) -> f (PlayerInfos, [Tile])) -> Game -> f Game
playersAndBag f game' = (\(_players, _bag) -> game' {_players, _bag}) <$> f (_players game', _bag game')
