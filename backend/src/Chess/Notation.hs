{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Chess.Notation (FullMove (..), fen, Result (..), MoveText (..), flattenMove, NotatedMove, fileChar, rankChar, rankChars, fileChars, pieceTypeChar, pieceTypeChars, pieceChars, FenElement (..), castleChar, castleChars, activeColourChar, notateMoves, fromMoves, PGN (..), printMove, MoveAndAppendation (..), fromFullMoves, legalMovesNotated) where

import Chess.Data
import Chess.Game (gameCheckType, legalMoves)
import Chess.Lib (Counts, getCount, guarded, one, singletonMaybe, traversefst, withInput)
import Control.Monad (guard, (>=>))
import Control.Monad.Reader (Reader, reader, runReader)
import Control.Monad.Writer (Writer, runWriter, writer)
import Data.Char (toLower)
import Data.Foldable (Foldable (toList))
import Data.Functor.Compose (Compose (..))
import Data.List as List (intercalate, singleton)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

data PGN = PGN {tags :: [(Text, Text)], moveText :: MoveText} deriving (Show)

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
notateFile file' = List.singleton $ fileChar file'

notatePiece :: PieceType -> String
notatePiece Pawn = ""
notatePiece piece' = [pieceTypeChar piece']

data FullMove = FullMove
  { moveNumber :: Int,
    whiteMove :: Maybe MoveAndAppendation,
    blackMove :: Maybe MoveAndAppendation
  }
  deriving (Eq, Show)

data MoveText = MoveText {fullMoves :: [FullMove], result :: Maybe Result} deriving (Eq, Show)

data Result = WinForWhite | WinForBlack | Draw deriving (Eq, Show)

fileChar :: File -> Char
fileChar file' = case file' of
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
rankChar rank' = case rank' of
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
pieceTypeChar piece' = case piece' of
  Knight -> 'N'
  Rook -> 'R'
  Bishop -> 'B'
  Queen -> 'Q'
  King -> 'K'
  Pawn -> 'P'

pieceTypeChars :: [(PieceType, Char)]
pieceTypeChars = map (withInput pieceTypeChar) pieceTypes

pieceChar :: Piece -> Char
pieceChar (White, piece') = pieceTypeChar piece'
pieceChar (Black, piece') = toLower $ pieceTypeChar piece'

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
  show (FenPiece piece') = [pieceChar piece']

fen :: Game -> String
fen (Game {playerToMove, gameBoard, castlesAvailable, enPassantSquare, halfMoveClock, fullMoveNumber}) = unwords [piecePlacement gameBoard, singleton (activeColourChar playerToMove), castlingAvailability castlesAvailable, notateSquare enPassantSquare, show halfMoveClock, show fullMoveNumber]

notateSquare :: Maybe Square -> String
notateSquare Nothing = "-"
notateSquare (Just (file', rank')) = [fileChar file', rankChar rank']

piecePlacement :: Board -> String
piecePlacement board = intercalate "/" $ map rankPlacement (reverse ranks)
  where
    rankPlacement rank' = concatMap show $ foldr f [] files
      where
        f file' acc = case (Map.lookup (file', rank') board, acc) of
          (Just piece', _) -> FenPiece piece' : acc
          (Nothing, NumberOfSquares n : rest) -> NumberOfSquares (n + 1) : rest
          (Nothing, _) -> NumberOfSquares 1 : acc

type WriterReader w a = Compose (Writer w) (Reader w) a

writerReader :: (Monoid w) => (w -> a, w) -> WriterReader w a
writerReader = Compose . writer . mapfst reader
  where
    mapfst g (a, w) = (g a, w)

runWriterReader :: WriterReader w a -> a
runWriterReader = uncurry runReader . runWriter . getCompose

notateMoves :: [(ChessMove CommonMove, Maybe CheckType)] -> [String]
notateMoves = runWriterReader . traverse (fmap (show . uncurry MoveAndAppendation) . traversefst notateMove)
  where
    notateMove :: ChessMove CommonMove -> WriterReader (Counts (PieceType, File), Counts (PieceType, Rank)) NotatedMove
    notateMove (RegularMove (CommonMove piece' (fromFile, fromRank) toSquare capturedPiece promotion)) = writerReader (regularNotatedMove . notateMoveValues . compress, (one (piece', fromFile), one (piece', fromRank)))
      where
        regularNotatedMove move = RegularMove (move {notatedPromotion = promotion})
        compress (files', ranks') = Movement (piece', (fromFile <$ guard fileDuplicated, fromRank <$ guard rankDuplicated)) (capturedPiece, toSquare)
          where
            fileDuplicated = getCount files' (piece', fromFile) > 1
            rankDuplicated = getCount ranks' (piece', fromRank) > 1
    notateMove (Castle side) = pure $ Castle side

legalMovesNotated :: Game -> [String]
legalMovesNotated = notateMoves . map (fmap gameCheckType) . legalMoves

notateMoveValues :: Movement (PieceType, (Maybe File, Maybe Rank)) (Maybe PieceType, Square) -> NotatedMoveValues
notateMoveValues (Movement (piece', from) (attacking, toSquare)) = NotatedMoveValues {notatedMovePiece = piece', notatedFrom = from, moveType, notatedToSquare = toSquare, notatedPromotion = Nothing}
  where
    moveType = case attacking of
      Just _ -> Takes
      Nothing -> To

data FullMoveError = FullMoveError {failingMoveNumber :: Int, failingPlayer :: Player, failingMove :: MoveAndAppendation} deriving (Eq, Show)

fromFullMoves :: [FullMove] -> Game -> Either FullMoveError Game
fromFullMoves = foldr ((>=>) . f) pure
  where
    f (FullMove {moveNumber, whiteMove, blackMove}) = maybe Right (apply White) whiteMove >=> maybe Right (apply Black) blackMove
      where
        apply player move = maybe (Left $ FullMoveError {failingMoveNumber = moveNumber, failingPlayer = player, failingMove = move}) Right . applyMoveAndAppendation move

fromMoves :: [MoveAndAppendation] -> Game -> Maybe Game
fromMoves = foldr ((>=>) . applyMoveAndAppendation) pure

applyMoveAndAppendation :: MoveAndAppendation -> Game -> Maybe Game
applyMoveAndAppendation (MoveAndAppendation notatedMove' appendation) game = do
  guarded matchesCheckType =<< singletonMaybe [newGame | (move, newGame) <- legalMoves game, matches notatedMove' move]
  where
    matchesCheckType game' = appendation == gameCheckType game'

matches :: NotatedMove -> ChessMove CommonMove -> Bool
matches (RegularMove (NotatedMoveValues p1 (maybeOriginFile, maybeOriginRank) x1 d1 pr1)) (RegularMove (CommonMove p2 (of2, or2) d2 x2 pr2)) =
  p1 == p2
    && ( case x1 of
           Takes -> isJust x2
           To -> isNothing x2
       )
    && d1 == d2
    && all (== of2) maybeOriginFile
    && all (== or2) maybeOriginRank
    && pr1 == pr2
matches (Castle l) (Castle r) = l == r
matches _ _ = False

data MoveAndAppendation = MoveAndAppendation NotatedMove (Maybe CheckType) deriving (Eq)

instance Show MoveAndAppendation where
  show = printMove
