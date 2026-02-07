{-# LANGUAGE NamedFieldPuns #-}

module Chess.Data where

import Control.Monad (guard, (<=<))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

type Board = Map Square Piece

forPlayer :: a -> a -> Player -> a
forPlayer white black player = case player of
  White -> white
  Black -> black

pieceLocations :: Player -> Board -> [(Square, PieceType)]
pieceLocations player board = [(square, pieceType) | (square, (player', pieceType)) <- Map.toList board, player == player']

fromPieces :: [(Square, Piece)] -> Board
fromPieces = Map.fromList

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

-- data Move a = Move {movePiece :: PieceType, fromSquare :: a, moveType :: MoveType, toSquare :: Square, checkStatus :: Maybe CheckType}

-- data PostMove = PostMove {move :: Move, postMoveBoard :: BoardStatus, gameStatus :: Maybe GameStatus}

data Move = Move (Movement (Maybe PieceType, Square)) | Castle CastleSide deriving (Show, Eq)

data Movement a = Movement {from :: (PieceType, Square), to :: a} deriving (Show, Eq, Ord)

type AttackingMove = Movement (PieceType, Square)

type SimpleMove = Movement Square

data NotatedMove = NotatedMove {notatedMovePiece :: PieceType, maybeFrom :: (Maybe File, Maybe Rank), moveType :: MoveType, notatedToSquare :: Square, notatedCheckStatus :: Maybe CheckType} deriving (Eq)

type AmbiguousSquare = (Maybe File, Maybe Rank)

data CheckType = Check | Mate deriving (Eq, Show)

data CheckStatus = CheckType CheckType | Stalemate deriving (Eq, Show)

checkType :: CheckStatus -> Maybe CheckType
checkType (CheckType checkType) = Just checkType
checkType _ = Nothing

-- moveIdentity :: PieceType -> Square -> MoveType -> Square -> Maybe CheckType -> Move
-- moveIdentity movePiece (fromFile, fromRank) moveType toSquare checkStatus = Move {movePiece, fromSquare, moveType, toSquare, checkStatus}
--  where
--    fromSquare = (Identity fromFile, Identity fromRank)

data MoveType = Takes | To deriving (Eq, Show)

data Game = Game {playerToMove :: Player, boardStatus :: BoardStatus, fullMoveNumber :: Int} deriving (Eq, Show)

data BoardStatus = BoardStatus {gameBoard :: Board, castlesAvailable :: Set CastleLocation, enPassantSquare :: Maybe Square, halfMoveClock :: Int} deriving (Eq, Show)

data CastleSide = Kingside | Queenside deriving (Eq, Ord, Show)

castleSides :: [CastleSide]
castleSides = [Kingside, Queenside]

type CastleLocation = (Player, CastleSide)

allCastles :: [(Player, CastleSide)]
allCastles = (,) <$> [White, Black] <*> castleSides
