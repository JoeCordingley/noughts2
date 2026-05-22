{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tigris.Game
  (
    setupGame,
    playGame
  )
where

import Control.Lens
import Data.Semigroup
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
import Data.Bool (bool)
import Control.Monad.State.Lazy (runState, State, state)
import BasicPrelude hiding ((<*))
import qualified Bag
import Data.Functor.Compose
import qualified Data.List as List


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
    fromShuffled dynasties' tiles = PlayingState (GameStage (cycle dynasties') (Turn FirstAction) ) (Game {_bag = remainingTiles, _players = fmap startingPlayerInfo startingPlayerHand, _board = startingBoard}) where
      (startingPlayerHand, remainingTiles) = fromJust $ state' traverse dealUpToSix (emptyHands, tiles)
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

playGame :: Monad m => (Dynasty -> Interactions m) -> (PlayingState -> m (Winners, Game)) -> PlayingState -> m (Winners, Game)
playGame interactions recurse (PlayingState gamestage game) = uncurry (either pure' recurse') =<< playInteraction interactions gamestage game where
  pure' winners' game' = pure (winners', game')
  recurse' gamestage' game' = recurse (PlayingState gamestage' game')

--playInteraction :: Monad m => (Dynasty -> Interactions m) -> Interaction -> [Dynasty] -> Game -> m (Either Winners PlayingState)
--playInteraction interactions' interaction (currentPlayer:subsequentPlayers) game = case interaction of
--  Turn turnNumber -> applyAction <$> getAction playerInteractions where
--    applyAction Pass = endTurn game
--    applyAction (ReplaceTiles discards) = continue =<< maybe (Left $ determineWinners game) Right ((playerAndBag . state (at currentPlayer . traverse . hand)) (dealUpToSix . (<>) discards) game)
--    applyAction PlayCatastrophe = undefined
--    applyAction (PlaceTile _ _) = undefined
--    applyAction (PositionLeader sphere leaderPosition) = trace "here" (continue . placeLeader currentPlayer sphere leaderPosition) game 
--    --        revolt dynasty = Right . PlayingState (RevoltAttack (RevoltDetails {_revoltDefender = dynasty, _revoltSphere = sphere})) (currentPlayer:subsequentPlayers)
--    nextPlayer = PlayingState (Turn FirstAction) subsequentPlayers 
--    endTurn game' = maybe (Left $ determineWinners game') (Right . nextPlayer) (guard continueIfEnoughTreasures *> refreshHands game') where
--      continueIfEnoughTreasures = view (board . numberOfTreasuresLeft) game' > 2
--    continue = case turnNumber of
--      FirstAction -> Right . PlayingState (Turn SecondAction) (currentPlayer:subsequentPlayers)
--      SecondAction -> endTurn
--    refreshHands = playerAndBag $ state (traverse . hand) dealUpToSix
--  RevoltAttack _ -> undefined
--  RevoltDefence _ -> undefined
--  where
--    playerInteractions = interactions' currentPlayer
--playInteraction _ _ [] _ = undefined

playInteraction :: Monad m => (Dynasty -> Interactions m) -> GameStage -> Game -> m (Either Winners GameStage, Game)
playInteraction interactions' (GameStage (currentPlayer:subsequentPlayers) interaction ) game = case interaction of
  Turn turnNumber -> flip runState game . applyAction <$> getAction playerInteractions where
    playerInteractions = interactions' currentPlayer
    applyAction Pass = endTurn
    applyAction (ReplaceTiles discards) = bool continue (Left <$> determineWinners) . isFinished =<< playersAndBag . onFirst (at currentPlayer . traverse . hand) %%= replaceTiles discards
    applyAction PlayCatastrophe = undefined
    applyAction (PlaceTile _ _) = undefined
    applyAction (PositionLeader sphere leaderPosition) = maybe continue (pure . Right . GameStage (currentPlayer:subsequentPlayers) . RevoltAttack) =<< positionLeader sphere leaderPosition
    positionLeader :: Sphere -> Maybe Position -> State Game (Maybe RevoltDetails)
    positionLeader sphere maybePosition = Nothing <$ placeLeader currentPlayer sphere maybePosition
    replaceTiles discards (hand', bag') = dealHand' (discards <> hand', bag')
    continue = case turnNumber of
      SecondAction -> endTurn
      FirstAction -> pure . Right $ GameStage (currentPlayer:subsequentPlayers) (Turn SecondAction) 
    endTurn = bool (Right <$> pure nextPlayer) (Left <$> determineWinners) . isFinished =<< orContinueWith refreshHands =<< finishedDueToTreasures
    refreshHands = playersAndBag . onFirst (traverse . hand) %%= dealHand'
    finishedDueToTreasures = uses (board . numberOfTreasuresLeft) (IsFinished . (<= 2))
    nextPlayer = GameStage subsequentPlayers (Turn FirstAction) 
    orContinueWith f = bool f (return $ IsFinished True) . isFinished
    dealHand' (hand', bag') = continueUntilFinished (6 - Bag.size hand') dealOne (hand', bag') 
--    nextPlayer = PlayingState (Turn FirstAction) subsequentPlayers 
--    playerInteractions = interactions' currentPlayer
--playInteraction _ _ [] _ = undefined

data DealState a = DealState { finishedState :: IsFinished, bagState :: [Tile], playerState :: a}

nest :: Monad m => Int -> (a -> m a) -> a -> m a
nest n f = foldr (>=>) return $ replicate n f

dealOne :: (Hand, [Tile]) -> (IsFinished, (Hand, [Tile]))
dealOne (hand', []) = (IsFinished True, (hand', []))
dealOne (hand', t:ts) = (IsFinished False, (hand' <> Bag.one t, ts))

continueUntilFinished :: Int -> (a -> (IsFinished, a)) -> a -> (IsFinished , a)
continueUntilFinished = nest
--foldCont = foldCont' (IsFinished False) where
--  foldCont' isF 0 _ a = (isF, a) 
--  foldCont (IsFinished True) _ _ a = ()
--  foldCont' isF n f a = foldCont' is

traverseUntilFinished :: Applicative f => (v -> f (IsFinished, v)) -> Map k v -> f (IsFinished, Map k v)
traverseUntilFinished = undefined

traverseUntilFinished2 :: (PlayerInfo -> Compose ((,) IsFinished) ((,) [Tile]) PlayerInfo)
             -> [PlayerInfo] -> Compose ((,) IsFinished) ((,) [Tile]) [PlayerInfo]
traverseUntilFinished2 f [] = Compose (IsFinished False, ([], []))
traverseUntilFinished2 f (a:as) = case f a of
  Compose (IsFinished True, (t, p)) -> Compose (IsFinished True, (t, p : as))
  Compose (IsFinished False, (t, p)) -> traverseUntilFinished2 f as


--traveerse f [] = pure []
--traveerse f (a:as) = (:) <$> f a <*> traverse f as

whenM :: (Applicative f, Monoid a) => f a -> Bool -> f a
whenM = bool (pure mempty) 

newtype IsFinished = IsFinished{isFinished :: Bool}
instance (Semigroup IsFinished) where 
  IsFinished x <> IsFinished y = IsFinished $ x || y
instance (Monoid IsFinished) where 
  mempty = IsFinished False

placeLeader :: Dynasty -> Sphere -> Maybe Position -> State Game ()
placeLeader dynasty sphere maybePosition = const (maybe addToHand addToBoard maybePosition) =<< maybe removeFromHand removeFromBoard =<< swapOutLeaderPosition where
  addToHand = playerLeadersInHand' %= Set.insert sphere
  addToBoard :: Position -> State Game ()
  addToBoard position = slot' position .= Just (LeaderPiece dynasty sphere)
  removeFromHand = playerLeadersInHand' %= Set.delete sphere
  removeFromBoard :: Position -> State Game ()
  removeFromBoard position = slot' position .= Nothing
  swapOutLeaderPosition = board . leaderPositions . at (Leader dynasty sphere) %%= (, maybePosition)
  playerLeadersInHand' = players . at dynasty . traverse . playerLeadersInHand
  slot' position = board . grid . ix position . slot

--placeLeader :: Dynasty -> Sphere -> Maybe Position -> Game -> Game
--placeLeader dynasty sphere maybePosition = maybe addToHand addToBoard maybePosition . uncurry (maybe removeFromHand removeFromBoard) . swapOutLeaderPosition where
--  swapOutLeaderPosition = (board . leaderPositions . at (Leader dynasty sphere)) (, maybePosition)
--  addToBoard position = set (slot' position) (Just $ LeaderPiece dynasty sphere) 
--  addToHand = over playerLeadersInHand' $ Set.insert sphere 
--  removeFromBoard position = set (slot' position) Nothing
--  removeFromHand = over playerLeadersInHand' $ Set.delete sphere
--  playerLeadersInHand' = players . at dynasty . traverse . playerLeadersInHand
--  slot' position = board . grid . ix position . slot

--playTurn :: (Monad f) => (Dynasty -> Game -> f (Either Game (Either Winners Game))) -> Dynasty -> Game -> f (Either Winners Game)
--playTurn getTurn player = runExceptT . (liftEither . (maybeEndGame <=< endTurn) <=< playUpToTwoTurns) where
--  playUpToTwoTurns = orReturnAfterPass . twice turn
--  orReturnAfterPass = ExceptT . fmap (either Right id) . runExceptT . runExceptT
--  turn = ExceptT . ExceptT . getTurn player


determineWinners :: State Game Winners 
determineWinners = undefined
--determineWinners :: Game -> Winners 
--determineWinners = winners . fmap (view score) . view players 

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

state' :: ((a -> StateT st f b) -> s -> StateT st f t) -> (a -> StateT st f b) -> (s, st) -> f (t, st)
state' l f (s, st) = runStateT (l f s) st

dealUpToSix :: Hand -> StateT [Sphere] Maybe Hand
dealUpToSix playerTiles = foldr (>=>) pure (replicate (6 - total playerTiles) (StateT . dealOne)) playerTiles where
  dealOne playerTiles' (x:xs) = Just (one x <> playerTiles', xs)
  dealOne _ [] = Nothing

total :: Bag a -> Int
total = getSum . fold


playersAndBag :: Functor f => ((PlayerInfos, [Tile]) -> f (PlayerInfos, [Tile])) -> Game -> f Game
playersAndBag f game' = (\(_players, _bag) -> game'{_players, _bag}) <$> f (_players game', _bag game')


onSecond :: ((a -> Compose f g b) -> s -> Compose f g t) -> ((c, a) -> f (g b)) -> (c, s) -> f (g t)
onSecond l f (c, s) = getCompose $ l g s where
  g a = Compose $ f (c, a)


onFirst :: ((a -> StateT c f b) -> s -> StateT c f t) -> ((a, c) -> f (b, c)) -> (s, c) -> f (t, c)
onFirst l f (s, c) = runStateT (l g s) c where
  g a = StateT (curry f a)



