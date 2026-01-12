{-# LANGUAGE NamedFieldPuns #-}

module Chess.Notation.Parser where

import Chess.Data
import Chess.Notation
import Control.Applicative (optional, (<|>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, many, token)
import Text.Megaparsec.Char (char, hspace)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ErrorItem (..))

type Stream = String

type Error = Void

type Parser a = Parsec Error Stream a

moveText :: Parser MoveText
moveText = MoveText <$> many (try fullMove) <*> optional result

result :: Parser Result
result = WinForWhite <$ "1-0" <|> WinForBlack <$ "0-1" <|> Draw <$ "1/2-1/2"

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

piece :: Parser PieceType
piece = charMap pieceMap <|> pure Pawn
  where
    pieceMap =
      Map.fromList $ mapMaybe f pieceTypes
      where
        f Pawn = Nothing
        f pieceType = Just (pieceTypeChar pieceType, pieceType)

parseDots :: Parser Dots
parseDots = lexeme (ThreeDots <$ "..." <|> OneDot <$ ".")

lexeme :: Parser a -> Parser a
lexeme p = p <* hspace

file :: Parser File
file = charMap . Map.fromList $ map (\file -> (fileChar file, file)) files

rank :: Parser Rank
rank = charMap . Map.fromList $ map (\rank -> (rankChar rank, rank)) ranks

data Dots = OneDot | ThreeDots
