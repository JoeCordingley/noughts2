{-# LANGUAGE NamedFieldPuns #-}

module Chess.Data (Board, File (..), PieceType (..), Rank (..), Square, Player (..), Piece, Move (..), MoveType (..), Game (..), CastleLocation (..), files, ranks, pieceTypes, allCastles, CastleSide (..), moveIdentity, fromSquareIdentity) where

import Control.Monad.Identity (Identity (Identity))
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

files :: [File]
files = [A, B, C, D, E, F, G, H]

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

ranks :: [Rank]
ranks = [One, Two, Three, Four, Five, Six, Seven, Eight]

data PieceType
  = King
  | Rook
  | Knight
  | Bishop
  | Queen
  | Pawn
  deriving (Eq, Ord)

pieceTypes :: [PieceType]
pieceTypes = [King, Queen, Rook, Bishop, Knight, Pawn]

type Piece =
  (Player, PieceType)

type Square = (File, Rank)

data Player
  = White
  | Black
  deriving (Show, Eq, Ord)

data Move f = Move {movePiece :: PieceType, fromSquareF :: (f File, f Rank), moveType :: MoveType, toSquare :: Square}

moveIdentity :: PieceType -> Square -> MoveType -> Square -> Move Identity
moveIdentity movePiece (fromFile, fromRank) moveType toSquare = Move {movePiece, fromSquareF, moveType, toSquare}
  where
    fromSquareF = (Identity fromFile, Identity fromRank)

fromSquareIdentity :: (Identity File, Identity Rank) -> Square
fromSquareIdentity (Identity file, Identity rank) = (file, rank)

data MoveType = Takes | To deriving (Eq)

data Game = Game {gameBoard :: Board, playerToMove :: Player, castlesAvailable :: Set CastleLocation, enPassantSquare :: Maybe Square, halfMoveClock :: Int, fullMoveNumber :: Int}

data CastleSide = Kingside | Queenside deriving (Eq, Ord)

type CastleLocation = (Player, CastleSide)

allCastles :: [(Player, CastleSide)]
allCastles = (,) <$> [White, Black] <*> [Kingside, Queenside]
