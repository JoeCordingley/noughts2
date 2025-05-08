{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Noughts.Game (WinningLine, GameInPlay (..), Game (..), GetMove, Player (..), Board, Space, boardLens, Result (..), Grid (..), Row (..), Move (..), playUnfixed, fixPlay, FinishedGame (..)) where

import BasicPrelude
import Control.Lens
import Data.Aeson (FromJSON (..), ToJSON)
import Data.Function (fix)
import Data.Semigroup
import GHC.Generics (Generic)

type RecursiveUnfixed a f b = (a -> f b) -> a -> f b

data Grid a = Grid {_top :: Row a, _middle :: Row a, _bottom :: Row a}

data Row a = Row {_left :: a, _center :: a, _right :: a}

makeLenses ''Row
makeLenses ''Grid

fixPlay :: RecursiveUnfixed GameInPlay f FinishedGame -> f FinishedGame
fixPlay unfixed = fix unfixed startingGame

playUnfixed :: (Monad f) => GetMove f -> RecursiveUnfixed GameInPlay f FinishedGame
playUnfixed getMove recurse gameInPlay = do
  status <- playTurn
  case status of
    Playing newGame -> recurse newGame
    Finished result -> pure result
  where
    playTurn = getStatus <$> getMove gameInPlay
    getStatus move = case status of
      Unfinished nextPlayer -> Playing $ GameInPlay nextPlayer newBoard
      FinishedStatus result -> Finished $ FinishedGame newBoard result
      where
        newBoard = set (boardLens move) (Just player) board
        status = postTurnStatus player newBoard
    GameInPlay player board = gameInPlay

postTurnStatus :: Player -> Board -> GameStatus
postTurnStatus player board = case wonGame of
  Just wonLines -> FinishedStatus $ WonGame player wonLines
  Nothing -> if all marked board then FinishedStatus DrawnGame else Unfinished (switch player)
  where
    wonGame = foldMap (fmap singleton . winningLine) winningLines
    winningLine spaces =
      if all markedByThisPlayer spaces
        then Just spaces
        else Nothing
      where
        markedByThisPlayer space = view (boardLens space) board == Just player

marked :: Space -> Bool
marked = isJust

switch :: Player -> Player
switch O = X
switch X = O

winningLines :: [WinningLine]
winningLines =
  [ [NW, N, NE],
    [W, C, E],
    [SW, S, SE],
    [NW, W, SW],
    [N, C, S],
    [NE, E, SE],
    [NW, C, SE],
    [NE, C, SW]
  ]

type GetMove f = GameInPlay -> f Move

data GameInPlay = GameInPlay Player Board

data Game = Playing GameInPlay | Finished FinishedGame

data Player = O | X deriving (Eq)

data GameStatus = Unfinished Player | FinishedStatus Result

data FinishedGame = FinishedGame Board Result

data Result = WonGame Player [WinningLine] | DrawnGame

type WinningLine = [Move]

data Move = NW | N | NE | W | C | E | SW | S | SE deriving (Show, Eq, Generic)

type Board = Grid Space

startingGame :: GameInPlay
startingGame = GameInPlay O startingBoard

startingBoard :: Board
startingBoard = pure Nothing

instance Functor Row where
  fmap f (Row l c r) = Row (f l) (f c) (f r)

instance Applicative Row where
  pure a = Row a a a
  Row fl fc fr <*> Row l c r = Row (fl l) (fc c) (fr r)

instance Functor Grid where
  fmap f (Grid t m b) = Grid (fmap f t) (fmap f m) (fmap f b)

instance Applicative Grid where
  pure a = Grid (pure a) (pure a) (pure a)
  Grid ft fm fb <*> Grid t m b = Grid (ft <*> t) (fm <*> m) (fb <*> b)

instance Foldable Row where
  foldMap f (Row l c r) = f l <> f c <> f r

instance Foldable Grid where
  foldMap f (Grid t m b) = foldMap f t <> foldMap f m <> foldMap f b

type Space = Maybe Player

type BoardLens = forall a. Lens' (Grid a) a

boardLens :: Move -> BoardLens
boardLens NW = top . left
boardLens N = top . center
boardLens NE = top . right
boardLens W = middle . left
boardLens C = middle . center
boardLens E = middle . right
boardLens SW = bottom . left
boardLens S = bottom . center
boardLens SE = bottom . right

instance FromJSON Move

instance ToJSON Move

instance (Semigroup a) => Semigroup (Grid a) where
  l <> r = (<>) <$> l <*> r

instance (Monoid a) => Monoid (Grid a) where
  mempty = pure mempty
