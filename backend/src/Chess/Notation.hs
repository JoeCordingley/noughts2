{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Chess.Notation (FullMove (..), fen, Result (..), MoveText (..), flattenMove, NotatedMove (..), fileChar, rankChar, rankChars, fileChars, pieceTypeChar, pieceTypeChars, pieceChars, FenElement (..), castleChar, castleChars, activeColourChar) where

import Chess.Data
import Chess.Lib (withInput)
import Data.Char (toLower)
import Data.Foldable (Foldable (toList))
import Data.List as List (intercalate, singleton)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

flattenMove :: FullMove -> [NotatedMove]
flattenMove (FullMove _ white black) = toList white ++ toList black

notateMove :: NotatedMove -> String
notateMove (NotatedMove movePiece (fromFile, fromRank) moveType (toFile, toRank) appendation) = notatePiece movePiece <> foldMap notateFile fromFile <> foldMap notateRank fromRank <> notateMoveType moveType <> notateFile toFile <> notateRank toRank <> foldMap notateAppendation appendation

notateAppendation :: CheckType -> String
notateAppendation Check = "+"
notateAppendation Mate = "#"

notateMoveType :: MoveType -> String
notateMoveType Takes = "x"
notateMoveType To = ""

notateRank :: Rank -> String
notateRank = List.singleton . rankChar

notateFile :: File -> String
notateFile file = List.singleton $ fileChar file

notatePiece :: PieceType -> String
notatePiece Pawn = ""
notatePiece piece = [pieceTypeChar piece]

instance Show NotatedMove where
  show = notateMove

data FullMove = FullMove
  { moveNumber :: Int,
    whiteMove :: Maybe NotatedMove,
    blackMove :: Maybe NotatedMove
  }
  deriving (Eq, Show)

data MoveText = MoveText [FullMove] (Maybe Result) deriving (Eq, Show)

data Result = WinForWhite | WinForBlack | Draw deriving (Eq, Show)

fileChar :: File -> Char
fileChar file = case file of
  A -> 'a'
  B -> 'b'
  C -> 'c'
  D -> 'd'
  E -> 'e'
  F -> 'f'
  G -> 'g'
  H -> 'h'

fileChars :: [(File, Char)]
fileChars = map (withInput fileChar) files

rankChar :: Rank -> Char
rankChar rank = case rank of
  One -> '1'
  Two -> '2'
  Three -> '3'
  Four -> '4'
  Five -> '5'
  Six -> '6'
  Seven -> '7'
  Eight -> '8'

rankChars :: [(Rank, Char)]
rankChars = map (withInput rankChar) ranks

pieceTypeChar :: PieceType -> Char
pieceTypeChar piece = case piece of
  Knight -> 'N'
  Rook -> 'R'
  Bishop -> 'B'
  Queen -> 'Q'
  King -> 'K'
  Pawn -> 'P'

pieceTypeChars :: [(PieceType, Char)]
pieceTypeChars = map (withInput pieceTypeChar) pieceTypes

pieceChar :: Piece -> Char
pieceChar (White, piece) = pieceTypeChar piece
pieceChar (Black, piece) = toLower $ pieceTypeChar piece

pieceChars :: [((Player, PieceType), Char)]
pieceChars = map (withInput pieceChar) pieces

activeColourChar :: Player -> Char
activeColourChar White = 'w'
activeColourChar Black = 'b'

castlingAvailability :: Set CastleLocation -> String
castlingAvailability s = if Set.null s then "-" else map castleChar allCastles

castleChar :: (Player, CastleSide) -> Char
castleChar (White, Kingside) = 'K'
castleChar (White, Queenside) = 'Q'
castleChar (Black, Kingside) = 'k'
castleChar (Black, Queenside) = 'q'

castleChars :: [((Player, CastleSide), Char)]
castleChars = map (withInput castleChar) allCastles

data FenElement = NumberOfSquares Int | FenPiece Piece

instance Show FenElement where
  show (NumberOfSquares n) = show n
  show (FenPiece piece) = [pieceChar piece]

fen :: Game -> String
fen (Game {playerToMove, boardStatus = BoardStatus {gameBoard, castlesAvailable, enPassantSquare}, halfMoveClock, fullMoveNumber}) = unwords [piecePlacement gameBoard, singleton (activeColourChar playerToMove), castlingAvailability castlesAvailable, square enPassantSquare, show halfMoveClock, show fullMoveNumber]

square :: Maybe Square -> String
square Nothing = "-"
square (Just (file, rank)) = [fileChar file, rankChar rank]

piecePlacement :: Board -> String
piecePlacement (Board {squares}) = intercalate "/" $ map rankPlacement (reverse ranks)
  where
    rankPlacement rank = concatMap show $ foldr f [] files
      where
        f file acc = case (Map.lookup (file, rank) squares, acc) of
          (Just piece, _) -> FenPiece piece : acc
          (Nothing, NumberOfSquares n : rest) -> NumberOfSquares (n + 1) : rest
          (Nothing, _) -> NumberOfSquares 1 : acc
