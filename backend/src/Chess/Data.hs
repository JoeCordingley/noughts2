{-# LANGUAGE NamedFieldPuns #-}

module Chess.Data (Board, File (..), PieceType (..), Rank (..), Square, Player (..), Piece, Move (..), MoveType (..), Game (..), CastleLocation, files, ranks, pieceTypes, allCastles, CastleSide (..), pieces, CheckType (..), AmbiguousMove, FullyDefinedMove) where

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
  deriving (Eq, Ord, Enum, Bounded, Show)

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
  deriving (Eq, Ord, Enum, Bounded, Show)

ranks :: [Rank]
ranks = [One, Two, Three, Four, Five, Six, Seven, Eight]

data PieceType
  = King
  | Rook
  | Knight
  | Bishop
  | Queen
  | Pawn
  deriving (Eq, Ord, Show)

pieceTypes :: [PieceType]
pieceTypes = [King, Queen, Rook, Bishop, Knight, Pawn]

pieces :: [Piece]
pieces = (,) <$> players <*> pieceTypes

players :: [Player]
players = [White, Black]

type Piece =
  (Player, PieceType)

type Square = (File, Rank)

data Player
  = White
  | Black
  deriving (Show, Eq, Ord)

data Move a = Move {movePiece :: PieceType, fromSquare :: a, moveType :: MoveType, toSquare :: Square, checkStatus :: Maybe CheckType}

type FullyDefinedMove = Move Square

type AmbiguousMove = Move AmbiguousSquare

type AmbiguousSquare = (Maybe File, Maybe Rank)

data CheckType = Check | Mate deriving (Eq, Show)

-- moveIdentity :: PieceType -> Square -> MoveType -> Square -> Maybe CheckType -> Move
-- moveIdentity movePiece (fromFile, fromRank) moveType toSquare checkStatus = Move {movePiece, fromSquare, moveType, toSquare, checkStatus}
--  where
--    fromSquare = (Identity fromFile, Identity fromRank)

data MoveType = Takes | To deriving (Eq)

data Game = Game {gameBoard :: Board, playerToMove :: Player, castlesAvailable :: Set CastleLocation, enPassantSquare :: Maybe Square, halfMoveClock :: Int, fullMoveNumber :: Int} deriving (Eq, Show)

data CastleSide = Kingside | Queenside deriving (Eq, Ord, Show)

type CastleLocation = (Player, CastleSide)

allCastles :: [(Player, CastleSide)]
allCastles = (,) <$> [White, Black] <*> [Kingside, Queenside]
