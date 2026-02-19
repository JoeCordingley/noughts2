{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Chess.Notation (FullMove (..), fen, Result (..), MoveText (..), flattenMove, NotatedMove (..), fileChar, rankChar, rankChars, fileChars, pieceTypeChar, pieceTypeChars, pieceChars, FenElement (..), castleChar, castleChars, activeColourChar, notateMoves, fromMoves, PGN (..)) where

import Chess.Data
import Chess.Game (gameCheckType, legalMoves)
import Chess.Lib (guarded, singletonMaybe, withInput)
import Control.Monad (guard, (>=>))
import Control.Monad.Reader (Reader, reader, runReader)
import Control.Monad.Writer (Writer, runWriter, writer)
import Data.Char (toLower)
import Data.Foldable (Foldable (toList))
import Data.Functor.Compose (Compose (..))
import Data.List as List (intercalate, singleton)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

data PGN = PGN {tags :: [(Text, Text)], moveText :: MoveText}

flattenMove :: FullMove -> [MoveAndAppendation]
flattenMove (FullMove _ white black) = toList white ++ toList black

printMove :: MoveAndAppendation -> String
printMove (MoveAndAppendation move appendation) =
  ( case move of
      RegularMove (NotatedMoveValues movePiece (fromFile, fromRank) moveType (toFile, toRank) promotion) -> notatePiece movePiece <> foldMap notateFile fromFile <> foldMap notateRank fromRank <> notateMoveType moveType <> notateFile toFile <> notateRank toRank <> foldMap notatePromotion promotion
      Castle Queenside -> "O-O-O"
      Castle Kingside -> "O-O"
  )
    <> foldMap notateAppendation appendation

notateAppendation :: CheckType -> String
notateAppendation Check = "+"
notateAppendation Mate = "#"

notatePromotion :: PieceType -> String
notatePromotion pieceType = "=" <> [pieceTypeChar pieceType]

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

instance Show MoveAndAppendation where
  show = printMove

data FullMove = FullMove
  { moveNumber :: Int,
    whiteMove :: Maybe MoveAndAppendation,
    blackMove :: Maybe MoveAndAppendation
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
fen (Game {playerToMove, gameBoard, castlesAvailable, enPassantSquare, halfMoveClock, fullMoveNumber}) = unwords [piecePlacement gameBoard, singleton (activeColourChar playerToMove), castlingAvailability castlesAvailable, square enPassantSquare, show halfMoveClock, show fullMoveNumber]

square :: Maybe Square -> String
square Nothing = "-"
square (Just (file, rank)) = [fileChar file, rankChar rank]

piecePlacement :: Board -> String
piecePlacement board = intercalate "/" $ map rankPlacement (reverse ranks)
  where
    rankPlacement rank = concatMap show $ foldr f [] files
      where
        f file acc = case (Map.lookup (file, rank) board, acc) of
          (Just piece, _) -> FenPiece piece : acc
          (Nothing, NumberOfSquares n : rest) -> NumberOfSquares (n + 1) : rest
          (Nothing, _) -> NumberOfSquares 1 : acc

type WriterReader w a = Compose (Writer w) (Reader w) a

type Counts a = Map a Int

writerReader :: (Monoid w) => (w -> a, w) -> WriterReader w a
writerReader = Compose . writer . mapfst reader
  where
    mapfst g (a, w) = (g a, w)

runWriterReader :: WriterReader w a -> a
runWriterReader = uncurry runReader . runWriter . getCompose

notateMoves :: [(ChessMove MoveData, Maybe CheckType)] -> [String]
notateMoves = runWriterReader . traverse (fmap (show . uncurry MoveAndAppendation) . traversefst notateMove)
  where
    one a = Map.singleton a 1
    traversefst f (a, b) = (,b) <$> f a
    notateMove (RegularMovement (piece, (fromFile, fromRank)) to promotion) = writerReader (RegularMove . notateMoveValues . compress, (one (piece, fromFile), one (piece, fromRank)))
      where
        compress (files, ranks) = Movement (piece, (fromFile <$ guard fileDuplicated, fromRank <$ guard rankDuplicated)) to
          where
            fileDuplicated = files ! (piece, fromFile) > 1
            rankDuplicated = ranks ! (piece, fromRank) > 1

notateMoveValues :: Movement (PieceType, (Maybe File, Maybe Rank)) (Maybe PieceType, Square) -> NotatedMoveValues
notateMoveValues (Movement (piece, from) (attacking, toSquare)) = NotatedMoveValues {notatedMovePiece = piece, notatedFrom = from, moveType, notatedToSquare = toSquare, notatedPromotion = Nothing}
  where
    moveType = case attacking of
      Just _ -> Takes
      Nothing -> To

fromMoves :: [MoveAndAppendation] -> Game -> Maybe Game
fromMoves = foldr ((>=>) . f) pure
  where
    f (MoveAndAppendation notatedMove appendation) game = do
      guarded matchesCheckType =<< singletonMaybe [newGame | (move, newGame) <- legalMoves game, matches notatedMove move]
      where
        matchesCheckType game = appendation == gameCheckType game

matches :: NotatedMove -> ChessMove MoveData -> Bool
matches (RegularMove (NotatedMoveValues p1 (maybeOriginFile, maybeOriginRank) x1 d1 pr1)) (RegularMovement (p2, (of2, or2)) (x2, d2) pr2) =
  p1 == p2
    && ( case x1 of
           Takes -> isJust x2
           To -> isNothing x2
       )
    && d1 == d2
    && all (== of2) maybeOriginFile
    && all (== or2) maybeOriginRank
    && pr1 == pr2
