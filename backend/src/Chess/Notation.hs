{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Chess.Notation (moveText, FullMove (..), fen, Move (..), move, fullMove, piece, rank, file, square, Result (..), MoveText (..), flattenMove, Parser, NotatedMove (..), fromSquare) where

import Chess.Data
import Control.Applicative (optional, (<|>))
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, many, token)
import Text.Megaparsec.Char (char, hspace)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ErrorItem (..))

type Stream = String

type Error = Void

type Parser a = Parsec Error Stream a

flattenMove :: FullMove -> [Move Maybe]
flattenMove (FullMove _ white black) = (getNotatedMove <$> toList white) ++ (getNotatedMove <$> toList black)

newtype NotatedMove = NotatedMove {getNotatedMove :: Move (Maybe)}

notateMove :: Move (Maybe) -> String
notateMove (Move movePiece (fromFile, fromRank) moveType (toFile, toRank)) = notatePiece movePiece <> foldMap notateFile fromFile <> foldMap notateRank fromRank <> notateMoveType moveType <> notateFile toFile <> notateRank toRank

notateMoveType Takes = "x"
notateMoveType To = ""

notateRank :: Rank -> String
notateRank rank = case rank of
  One -> "1"
  Two -> "2"
  Three -> "3"
  Four -> "4"
  Five -> "5"
  Six -> "6"
  Seven -> "7"
  Eight -> "8"

notateFile :: File -> String
notateFile file = case file of
  A -> "a"
  B -> "b"
  C -> "c"
  D -> "d"
  E -> "e"
  F -> "f"
  G -> "g"
  H -> "h"

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

data MoveText = MoveText {moves :: [FullMove], result :: Maybe Result} deriving (Eq, Show)

data Result = WinForWhite | WinForBlack | Draw deriving (Eq, Show)

moveText :: Parser MoveText
moveText = MoveText <$> many (try fullMove) <*> optional parseResult

parseResult :: Parser Result
parseResult = WinForWhite <$ "1-0" <|> WinForBlack <$ "0-1" <|> Draw <$ "1/2-1/2"

fullMove :: Parser FullMove
fullMove = do
  n <- decimal
  dots <- parseDots
  white <- case dots of
    OneDot -> Just <$> lexeme move
    ThreeDots -> pure Nothing
  black <- optional $ lexeme move
  return $ FullMove n white black

move :: Parser NotatedMove
move = try disambiguated <|> simple
  where
    disambiguated = NotatedMove <$> (Move <$> piece <*> fuzzySquare <*> parseMoveType <*> square)
    simple = move' <$> piece <*> parseMoveType <*> square
      where
        move' movePiece moveType toSquare = NotatedMove $ Move {movePiece, fromSquare = (Nothing, Nothing), moveType, toSquare}

fuzzySquare :: Parser (Maybe File, Maybe Rank)
fuzzySquare = (,) <$> optional file <*> optional rank

parseMoveType :: Parser MoveType
parseMoveType = Takes <$ "x" <|> pure To

square :: Parser Square
square = (,) <$> file <*> rank

charMap :: Map Char a -> Parser a
charMap m = token test expected
  where
    test c = Map.lookup c m
    expected = Set.fromList [Tokens (c :| []) | c <- Map.keys m]

fileMap :: Map Char File
fileMap =
  Map.fromList
    [ ('a', A),
      ('b', B),
      ('c', C),
      ('d', D),
      ('e', E),
      ('f', F),
      ('g', G),
      ('h', H)
    ]

rankMap :: Map Char Rank
rankMap =
  Map.fromList
    [ ('1', One),
      ('2', Two),
      ('3', Three),
      ('4', Four),
      ('5', Five),
      ('6', Six),
      ('7', Seven),
      ('8', Eight)
    ]

pieceMap :: Map Char PieceType
pieceMap =
  Map.fromList
    [ ('N', Knight),
      ('R', Rook),
      ('B', Bishop),
      ('Q', Queen),
      ('K', King)
    ]

piece :: Parser PieceType
piece = charMap pieceMap <|> pure Pawn

file :: Parser File
file = charMap fileMap

rank :: Parser Rank
rank = charMap rankMap

data Dots = OneDot | ThreeDots

parseDots :: Parser Dots
parseDots = lexeme (ThreeDots <$ "..." <|> OneDot <$ ".")

lexeme :: Parser a -> Parser a
lexeme p = p <* hspace

fen :: Game -> String
fen = undefined
