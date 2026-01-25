{-# LANGUAGE NamedFieldPuns #-}

module Chess.Data where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

data Board = Board {squares :: Map Square Piece, whitePieceLocations :: Map Square PieceType, blackPieceLocations :: Map Square PieceType} deriving (Eq, Show)

applyMoveToBoard :: Player -> Move -> Board -> Board
applyMoveToBoard player (Move {fromSquare, toSquare, movePiece}) (Board {squares, whitePieceLocations, blackPieceLocations}) = Board {squares = squares', whitePieceLocations = whitePieceLocations', blackPieceLocations = blackPieceLocations'}
  where
    update a = Map.insert toSquare a . Map.delete fromSquare
    delete = Map.delete toSquare
    squares' = update (player, movePiece) squares
    whitePieceLocations' = forPlayer (update movePiece) delete player whitePieceLocations
    blackPieceLocations' = forPlayer delete (update movePiece) player blackPieceLocations

forPlayer :: a -> a -> Player -> a
forPlayer white black player = case player of
  White -> white
  Black -> black

pieceLocations :: Player -> Board -> Map Square PieceType
pieceLocations = forPlayer whitePieceLocations blackPieceLocations

fromPieces :: [(Square, Piece)] -> Board
fromPieces pieces = Board {squares, whitePieceLocations, blackPieceLocations}
  where
    squares = Map.fromList pieces
    whitePieceLocations = pieceLocations White
    blackPieceLocations = pieceLocations Black
    pieceLocations player = Map.fromList [(square, pieceType) | (square, (player', pieceType)) <- pieces, player' == player]

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

data Move = Move {movePiece :: PieceType, fromSquare :: Square, pieceUnderAttack :: Maybe PieceType, toSquare :: Square, checkStatus :: Maybe CheckType} deriving (Show)

data NotatedMove = NotatedMove {notatedMovePiece :: PieceType, maybeFrom :: (Maybe File, Maybe Rank), moveType :: MoveType, notatedToSquare :: Square, notatedCheckStatus :: Maybe CheckType} deriving (Eq)

type AmbiguousSquare = (Maybe File, Maybe Rank)

data CheckType = Check | Mate deriving (Eq, Show)

-- moveIdentity :: PieceType -> Square -> MoveType -> Square -> Maybe CheckType -> Move
-- moveIdentity movePiece (fromFile, fromRank) moveType toSquare checkStatus = Move {movePiece, fromSquare, moveType, toSquare, checkStatus}
--  where
--    fromSquare = (Identity fromFile, Identity fromRank)

data MoveType = Takes | To deriving (Eq, Show)

data Game = Game {gameBoard :: Board, playerToMove :: Player, castlesAvailable :: Set CastleLocation, enPassantSquare :: Maybe Square, halfMoveClock :: Int, fullMoveNumber :: Int} deriving (Eq, Show)

data CastleSide = Kingside | Queenside deriving (Eq, Ord, Show)

type CastleLocation = (Player, CastleSide)

allCastles :: [(Player, CastleSide)]
allCastles = (,) <$> [White, Black] <*> [Kingside, Queenside]
