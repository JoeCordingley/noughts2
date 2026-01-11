module Chess.Data (Board, File(..), PieceType(..), Rank(..), Space, Player(..), Piece) where

import Data.Map (Map)
type Board = Map Space Piece
data File
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
data Rank
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  deriving (Eq, Ord, Enum, Bounded, Show)

data PieceType
  = King
  | Rook
  | Knight
  | Bishop
  | Queen
  | Pawn
  deriving (Eq, Ord, Show)
type Piece =
  (Player, PieceType)


type Space = (File, Rank)
data Player
  = White
  | Black
  deriving (Show, Eq, Ord)
