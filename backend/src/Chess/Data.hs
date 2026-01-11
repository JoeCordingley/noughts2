module Chess.Data (Board, File (..), PieceType (..), Rank (..), Square, Player (..), Piece, Move (..), MoveType (..), Game (..), CastleLocation (..)) where

import Data.Map (Map)
import Data.Set (Set)

type Board = Map Square Piece

data File
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  deriving (Eq, Ord, Enum, Bounded)

data Rank
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  deriving (Eq, Ord, Enum, Bounded)

data PieceType
  = King
  | Rook
  | Knight
  | Bishop
  | Queen
  | Pawn
  deriving (Eq, Ord)

type Piece =
  (Player, PieceType)

type Square = (File, Rank)

data Player
  = White
  | Black
  deriving (Show, Eq, Ord)

data Move f = Move {movePiece :: PieceType, fromSquare :: (f File, f Rank), moveType :: MoveType, toSquare :: Square}

data MoveType = Takes | To deriving (Eq)

data Game = Game {board :: Board, playerToMove :: Player, castlesAvailable :: Set (Player, CastleLocation), enPassantSquare :: Maybe Square, halfMoveClock :: Int, fullMoveNumber :: Int}

data CastleLocation = Kingside | Queenside deriving (Eq, Ord)
