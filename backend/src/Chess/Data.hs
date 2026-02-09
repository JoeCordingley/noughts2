{-# LANGUAGE NamedFieldPuns #-}

module Chess.Data (NotatedMoveValues (..), NotatedMove (..), Square, Board, Move (..), Game (..), CastleSide (..), Player (..), CastleLocation, PieceType (..), Rank (..), File (..), MoveType (..), CheckType (..), MoveAndAppendation (..), Piece, SimpleMove, AttackingMove, CheckStatus (..), ranks, files, pieces, allCastles, fromAttackingMove, fromSimpleMove, castleSides, fromPieces, checkType, pieceLocations, pieceTypes, notatedMove) where

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

data Move = Move (PieceType, Square) (Maybe PieceType, Square) | Castle CastleSide deriving (Show, Eq)

fromAttackingMove :: ((PieceType, Square), (PieceType, Square)) -> Move
fromAttackingMove (from, (piece, to)) = Move from (Just piece, to)

fromSimpleMove :: ((PieceType, Square), Square) -> Move
fromSimpleMove (from, to) = Move from (Nothing, to)

type AttackingMove = ((PieceType, Square), (PieceType, Square))

type SimpleMove = ((PieceType, Square), Square)

data NotatedMove = NotatedMove NotatedMoveValues | NotatedCastle CastleSide deriving (Eq)

notatedMove :: PieceType -> (Maybe File, Maybe Rank) -> MoveType -> Square -> NotatedMove
notatedMove notatedMovePiece (fromFile, fromRank) moveType notatedToSquare = NotatedMove $ NotatedMoveValues {notatedMovePiece, fromFile, fromRank, moveType, notatedToSquare}

data NotatedMoveValues = NotatedMoveValues {notatedMovePiece :: PieceType, fromFile :: Maybe File, fromRank :: Maybe Rank, moveType :: MoveType, notatedToSquare :: Square} deriving (Eq)

data MoveAndAppendation = MoveAndAppendation NotatedMove (Maybe CheckType) deriving (Eq)

type AmbiguousSquare = (Maybe File, Maybe Rank)

data CheckType = Check | Mate deriving (Eq, Show)

data CheckStatus = CheckType CheckType | Stalemate deriving (Eq, Show)

checkType :: CheckStatus -> Maybe CheckType
checkType (CheckType c) = Just c
checkType _ = Nothing

data MoveType = Takes | To deriving (Eq, Show)

data Game = Game {gameBoard :: Board, playerToMove :: Player, castlesAvailable :: Set CastleLocation, enPassantSquare :: Maybe Square, halfMoveClock :: Int, fullMoveNumber :: Int} deriving (Eq, Show)

data CastleSide = Kingside | Queenside deriving (Eq, Ord, Show)

castleSides :: [CastleSide]
castleSides = [Kingside, Queenside]

type CastleLocation = (Player, CastleSide)

allCastles :: [(Player, CastleSide)]
allCastles = (,) <$> [White, Black] <*> castleSides
