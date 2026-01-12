{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Chess.Notation (FullMove (..), fen, Move (..), Result (..), MoveText (..), flattenMove, NotatedMove (..), fromSquare, fileChar, rankChar, pieceTypeChar) where

import Chess.Data
import Control.Applicative (optional, (<|>))
import Data.Char (toLower)
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.List as List (intercalate, singleton)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)


flattenMove :: FullMove -> [Move Maybe]
flattenMove (FullMove _ white black) = (getNotatedMove <$> toList white) ++ (getNotatedMove <$> toList black)

newtype NotatedMove = NotatedMove {getNotatedMove :: Move (Maybe)}

notateMove :: Move (Maybe) -> String
notateMove (Move movePiece (fromFile, fromRank) moveType (toFile, toRank)) = notatePiece movePiece <> foldMap notateFile fromFile <> foldMap notateRank fromRank <> notateMoveType moveType <> notateFile toFile <> notateRank toRank

notateMoveType Takes = "x"
notateMoveType To = ""

notateRank :: Rank -> String
notateRank = List.singleton . rankChar

notateFile :: File -> String
notateFile file = List.singleton $ fileChar file

notatePiece :: PieceType -> String
notatePiece piece = case piece of
  King -> "K"
  Queen -> "Q"
  Rook -> "R"
  Bishop -> "B"
  Knight -> "N"
  Pawn -> ""

instance Show (NotatedMove) where
  show (NotatedMove move) = notateMove move

instance Eq (NotatedMove) where
  NotatedMove (Move p1 f1 x1 t1) == NotatedMove (Move p2 f2 x2 t2) = (p1 == p2) && (f1 == f2) && (x1 == x2) && (t1 == t2)

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

pieceTypeChar :: PieceType -> Char
pieceTypeChar piece = case piece of
  Knight -> 'N'
  Rook -> 'R'
  Bishop -> 'B'
  Queen -> 'Q'
  King -> 'K'
  Pawn -> 'P'

pieceChar :: Piece -> Char
pieceChar (White, piece) = pieceTypeChar piece
pieceChar (Black, piece) = toLower $ pieceTypeChar piece

activeColour :: Player -> String
activeColour White = "w"
activeColour Black = "b"

castlingAvailability :: Set CastleLocation -> String
castlingAvailability s = if Set.null s then "-" else concatMap castleChar allCastles
  where
    castleChar :: CastleLocation -> String
    castleChar (White, Kingside) = "K"
    castleChar (White, Queenside) = "Q"
    castleChar (Black, Kingside) = "k"
    castleChar (Black, Queenside) = "q"

data FenElement = NumberOfSquares Int | FenPiece Piece

instance Show FenElement where
  show (NumberOfSquares n) = show n
  show (FenPiece piece) = [pieceChar piece]

fen :: Game -> String
fen (Game {board, playerToMove, castlesAvailable, enPassantSquare, halfMoveClock, fullMoveNumber}) = intercalate " " [piecePlacement board, activeColour playerToMove, castlingAvailability castlesAvailable, square enPassantSquare, show halfMoveClock, show fullMoveNumber]

square :: Maybe Square -> String
square Nothing = "-"
square (Just (file, rank)) = [fileChar file, rankChar rank]

piecePlacement :: Board -> String
piecePlacement board = intercalate "/" $ map rankPlacement (reverse ranks)
  where
    rankPlacement :: Rank -> String
    rankPlacement rank = concatMap show $ foldr f [] files
      where
        f :: File -> [FenElement] -> [FenElement]
        f file acc = case Map.lookup (file, rank) board of
          Just piece -> (FenPiece piece) : acc
          Nothing -> case acc of
            NumberOfSquares n : rest -> NumberOfSquares (n + 1) : rest
            _ -> NumberOfSquares 1 : acc
