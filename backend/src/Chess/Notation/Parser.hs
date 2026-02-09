{-# LANGUAGE NamedFieldPuns #-}

module Chess.Notation.Parser (Parser, fen, fullMove, parseMove, moveText, parseMoveWithAppendation) where

import Chess.Data hiding (move)
import Chess.Lib (withInput)
import Chess.Notation hiding (fen)
import Control.Applicative (optional, (<|>))
import Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, choice, many, some)
import Text.Megaparsec.Char (char, hspace)
import Text.Megaparsec.Char.Lexer (decimal)

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
    OneDot -> Just <$> lexeme parseMoveWithAppendation
    ThreeDots -> pure Nothing
  black <- optional $ lexeme parseMoveWithAppendation
  return $ FullMove n white black

parseMoveWithAppendation :: Parser MoveAndAppendation
parseMoveWithAppendation = MoveAndAppendation <$> parseMove <*> optional appendation

parseMove :: Parser NotatedMove
parseMove = NotatedMove <$> (try disambiguated <|> simple)
  where
    disambiguated = NotatedMoveValues <$> pieceType <*> optional file <*> optional rank <*> parseMoveType <*> square
    simple = move' <$> pieceType <*> parseMoveType <*> square
      where
        move' notatedMovePiece moveType notatedToSquare = NotatedMoveValues {notatedMovePiece, fromFile = Nothing, fromRank = Nothing, moveType, notatedToSquare}

appendation :: Parser CheckType
appendation = (Check <$ char '+') <|> (Mate <$ char '#')

parseMoveType :: Parser MoveType
parseMoveType = Takes <$ "x" <|> pure To

square :: Parser Square
square = (,) <$> file <*> rank

pieceType :: Parser PieceType
pieceType = choice (map charMapping pieceTypeChars) <|> pure Pawn

parseDots :: Parser Dots
parseDots = lexeme (ThreeDots <$ "..." <|> OneDot <$ ".")

lexeme :: Parser a -> Parser a
lexeme p = p <* hspace

file :: Parser File
file = choice $ map charMapping fileChars

rank :: Parser Rank
rank = choice $ map charMapping rankChars

charMapping :: (a, Char) -> Parser a
charMapping (a, c) = a <$ char c

data Dots = OneDot | ThreeDots

fen :: Parser Game
fen = Game <$> lexeme piecePlacement <*> lexeme activeColourParser <*> lexeme castlingAvailability <*> lexeme enPassantTargetSquare <*> lexeme halfMoveClock <*> lexeme fullMoveNumber
  where
    piecePlacement = fmap (fromPieces . concat) . sequence . intersperse ([] <$ char '/') . map parseRank $ reverse ranks
    parseRank rank = parseRank' files
      where
        parseRank' [] = pure []
        parseRank' (file : files) = do
          fenElement <- parseFenElement
          case fenElement of
            FenPiece piece -> (((file, rank), piece) :) <$> parseRank' files
            NumberOfSquares n -> parseRank' (drop (n - 1) files)
    activeColourParser = choice $ map (charMapping . withInput activeColourChar) [White, Black]
    castlingAvailability = Set.empty <$ char '-' <|> Set.fromList <$> some castleLocation
    castleLocation = choice $ map charMapping castleChars
    enPassantTargetSquare = Nothing <$ char '-' <|> Just <$> square
    halfMoveClock = decimal
    fullMoveNumber = decimal

piece :: Parser Piece
piece = choice $ map charMapping pieceChars

parseFenElement :: Parser FenElement
parseFenElement = NumberOfSquares <$> decimal <|> FenPiece <$> piece
