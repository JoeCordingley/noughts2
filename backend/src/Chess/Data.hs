{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Chess.Data (NotatedMoveValues (..), MoveAndPromotion (..), NotatedMove, Square, Board, ChessMove (..), Game (..), CastleSide (..), Player (..), CastleLocation, PieceType (..), Rank (..), File (..), MoveType (..), CheckType (..), Piece, NonAttackingMove, AttackingMove, CheckStatus (..), ranks, files, pieces, allCastles, fromAttackingMove, fromNonAttackingMove, castleSides, fromPieces, checkType, pieceLocations, pieceTypes, notatedMove, CommonMove (..), Movement (..), attackingMove, nonAttackingMove, filesFrom, ranksFrom, EnPassantMove, fromEnPassantMove, majorPieces, piece, rank, square) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

type Board = Map Square Piece

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
  deriving (Eq, Ord, Enum, Show)

files :: [File]
files = [A, B, C, D, E, F, G, H]

nextFile :: File -> Maybe File
nextFile A = Just B
nextFile B = Just C
nextFile C = Just D
nextFile D = Just E
nextFile E = Just F
nextFile F = Just G
nextFile G = Just H
nextFile H = Nothing

previousFile :: File -> Maybe File
previousFile A = Nothing
previousFile B = Just A
previousFile C = Just B
previousFile D = Just C
previousFile E = Just D
previousFile F = Just E
previousFile G = Just F
previousFile H = Just G

nextRank :: Rank -> Maybe Rank
nextRank One = Just Two
nextRank Two = Just Three
nextRank Three = Just Four
nextRank Four = Just Five
nextRank Five = Just Six
nextRank Six = Just Seven
nextRank Seven = Just Eight
nextRank Eight = Nothing

previousRank :: Rank -> Maybe Rank
previousRank One = Nothing
previousRank Two = Just One
previousRank Three = Just Two
previousRank Four = Just Three
previousRank Five = Just Four
previousRank Six = Just Five
previousRank Seven = Just Six
previousRank Eight = Just Seven

iterateFrom :: (a -> Maybe a) -> a -> [a]
iterateFrom next = maybe [] extend . next
  where
    extend a = a : iterateFrom next a

filesFrom :: Ordering -> File -> [File]
filesFrom GT = iterateFrom nextFile
filesFrom EQ = iterateFrom Just
filesFrom LT = iterateFrom previousFile

ranksFrom :: Ordering -> Rank -> [Rank]
ranksFrom GT = iterateFrom nextRank
ranksFrom EQ = iterateFrom Just
ranksFrom LT = iterateFrom previousRank

data Rank
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  deriving (Eq, Ord, Enum, Show)

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

majorPieces :: [PieceType]
majorPieces = [Rook, Knight, Bishop, Queen]

pieceTypes :: [PieceType]
pieceTypes = [King, Queen, Rook, Bishop, Knight, Pawn]

pieces :: [Piece]
pieces = (,) <$> players <*> pieceTypes

players :: [Player]
players = [White, Black]

type Piece =
  (Player, PieceType)

type Square = (File, Rank)

piece :: (PieceType, b) -> PieceType
piece = fst

rank :: Square -> Rank
rank = snd

square :: (a, Square) -> Square
square = snd

data Player
  = White
  | Black
  deriving (Show, Eq, Ord)

data MoveAndPromotion a = MoveAndPromotion {move :: a, promotion :: Maybe PieceType} deriving (Show, Eq)

data ChessMove a = RegularMove a | Castle CastleSide deriving (Show, Eq)

data CommonMove = CommonMove
  { movingPiece :: PieceType
  , fromSquare :: Square
  , toSquare :: Square
  , capturedPiece :: Maybe PieceType
  }
  deriving (Eq, Show)

data Movement a b = Movement {from :: a, to :: b} deriving (Eq, Show)

fromAttackingMove :: AttackingMove -> CommonMove
fromAttackingMove (Movement (piece, from) (capture, to)) = CommonMove piece from to (Just capture)

fromNonAttackingMove :: NonAttackingMove -> CommonMove
fromNonAttackingMove (Movement (piece, from) to) = CommonMove piece from to Nothing

type AttackingMove = Movement (PieceType, Square) (PieceType, Square)

type EnPassantMove = Movement Square Square

fromEnPassantMove :: EnPassantMove -> CommonMove
fromEnPassantMove (Movement from to) = CommonMove Pawn from to (Just Pawn)

attackingMove :: PieceType -> Square -> PieceType -> Square -> AttackingMove
attackingMove piece from attacking at = Movement (piece, from) (attacking, at)

nonAttackingMove :: PieceType -> Square -> Square -> NonAttackingMove
nonAttackingMove piece from to = Movement (piece, from) to

type NonAttackingMove = Movement (PieceType, Square) Square

type NotatedMove = ChessMove (MoveAndPromotion NotatedMoveValues)

notatedMove :: PieceType -> (Maybe File, Maybe Rank) -> MoveType -> Square -> NotatedMove
notatedMove notatedMovePiece notatedFrom moveType notatedToSquare = RegularMove (MoveAndPromotion {move = NotatedMoveValues {notatedMovePiece, notatedFrom, moveType, notatedToSquare}, promotion = Nothing})

data NotatedMoveValues = NotatedMoveValues {notatedMovePiece :: PieceType, notatedFrom :: (Maybe File, Maybe Rank), moveType :: MoveType, notatedToSquare :: Square} deriving (Eq)

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
