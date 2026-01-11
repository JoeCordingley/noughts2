{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Chess.Notation (moveText, FullMove (..), fen, Move (..), move, fullMove, piece, rank, file, space, isCapture, Result (..), MoveText (..), flattenMove, Parser) where

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

flattenMove :: FullMove -> [Move]
flattenMove (FullMove _ white black) = toList white ++ toList black

data Move = Move {movePiece :: PieceType, fromFile :: Maybe File, fromRank :: Maybe Rank, isCapture :: Bool, toSpace :: Space} deriving (Eq, Show)

data FullMove = FullMove
  { moveNumber :: Int,
    whiteMove :: Maybe Move,
    blackMove :: Maybe Move
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

move :: Parser Move
move = try disambiguated <|> simple
  where
    disambiguated = Move <$> piece <*> optional file <*> optional rank <*> capture <*> space
    simple = move' <$> piece <*> capture <*> space
      where
        move' movePiece isCapture toSpace = Move {movePiece, fromFile = Nothing, fromRank = Nothing, isCapture, toSpace}

capture :: Parser Bool
capture = True <$ "x" <|> pure False

space :: Parser Space
space = (,) <$> file <*> rank

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

fen :: Board -> String
fen = undefined
