{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Chess.Notation.Parser (Parser, fen, fullMove, parseMove, parseMoveText, parseMoveWithAppendation, parsePgn, lexeme, parseMoveString) where

import Chess.Data
import Chess.Lib (withInput)
import Chess.Notation hiding (fen)
import Control.Applicative (many, optional, some, (<|>))
import Data.Attoparsec.Text as Parser (Parser, char, choice, decimal, endOfLine, skipSpace, takeWhile, takeWhile1, try)
import Data.Char (isLetter)
import Data.List (intersperse)
import qualified Data.Set as Set
import Data.Text (Text, unpack)

parseMoveText :: Parser MoveText
parseMoveText = MoveText <$> some (try fullMove) <*> parseResult

parseResult :: Parser (Maybe Result)
parseResult = Just WinForWhite <$ "1-0" <|> Just WinForBlack <$ "0-1" <|> Just Draw <$ "1/2-1/2" <|> Nothing <$ "*"

fullMove :: Parser FullMove
fullMove = do
  n <- decimal
  dots <- parseDots
  white <- case dots of
    OneDot -> Just <$> lexeme parseMoveWithAppendation
    ThreeDots -> pure Nothing
  black <- optional . try $ lexeme parseMoveWithAppendation
  return $ FullMove n white black

parsePgn :: Parser PGN
parsePgn = PGN <$> lexeme (many (tagPair <* endOfLine)) <*> lexeme parseMoveText

parseMoveWithAppendation :: Parser MoveAndAppendation
parseMoveWithAppendation = MoveAndAppendation <$> parseMove <*> optional appendation

parseMove :: Parser NotatedMove
parseMove = (RegularMove <$> (setPromotion <$> (disambiguated <|> simple) <*> optional parsePromotion)) <|> Castle <$> parseCastle
  where
    setPromotion move' p = move' {notatedPromotion = p}
    disambiguated = NotatedMoveValues <$> pieceType <*> fuzzySquare <*> parseMoveType <*> parseSquare <*> pure Nothing
    fuzzySquare = (,) <$> optional parseFile <*> optional parseRank
    simple = move' <$> pieceType <*> parseMoveType <*> parseSquare
      where
        move' notatedMovePiece moveType notatedToSquare = NotatedMoveValues {notatedMovePiece, notatedFrom = (Nothing, Nothing), moveType, notatedToSquare, notatedPromotion = Nothing}

parseMoveString :: Parser String
parseMoveString = unpack <$> takeWhile1 (`Set.member` moveChars)
  where
    moveChars = Set.fromList (map pieceTypeChar pieceTypes) <> Set.fromList (map fileChar files) <> Set.fromList (map rankChar ranks) <> Set.fromList ['x', '=', '+', '#', 'O', '-']

parseCastle :: Parser CastleSide
parseCastle = Queenside <$ "O-O-O" <|> Kingside <$ "O-O"

appendation :: Parser CheckType
appendation = (Check <$ char '+') <|> (Mate <$ char '#')

parsePromotion :: Parser PieceType
parsePromotion = "=" *> pieceType

parseMoveType :: Parser MoveType
parseMoveType = Takes <$ "x" <|> pure To

parseSquare :: Parser Square
parseSquare = (,) <$> parseFile <*> parseRank

pieceType :: Parser PieceType
pieceType = choice (map charMapping pieceTypeChars) <|> pure Pawn

parseDots :: Parser Dots
parseDots = lexeme (ThreeDots <$ "..." <|> OneDot <$ ".")

lexeme :: Parser a -> Parser a
lexeme p = p <* skipSpace

parseFile :: Parser File
parseFile = choice $ map charMapping fileChars

parseRank :: Parser Rank
parseRank = choice $ map charMapping rankChars

charMapping :: (a, Char) -> Parser a
charMapping (a, c) = a <$ char c

data Dots = OneDot | ThreeDots

fen :: Parser Game
fen = Game <$> lexeme piecePlacement <*> lexeme activeColourParser <*> lexeme castlingAvailability <*> lexeme enPassantTargetSquare <*> lexeme halfMoveClock <*> lexeme fullMoveNumber
  where
    piecePlacement = fmap (fromPieces . concat) . sequence . intersperse ([] <$ char '/') . map parseRank' $ reverse ranks
    parseRank' rank' = parseFiles files
      where
        parseFiles [] = pure []
        parseFiles (file' : files') = do
          fenElement <- parseFenElement
          case fenElement of
            FenPiece piece' -> (((file', rank'), piece') :) <$> parseFiles files'
            NumberOfSquares n -> parseFiles (drop (n - 1) files')
    activeColourParser = choice $ map (charMapping . withInput activeColourChar) [White, Black]
    castlingAvailability = Set.empty <$ char '-' <|> Set.fromList <$> some castleLocation
    castleLocation = choice $ map charMapping castleChars
    enPassantTargetSquare = Nothing <$ char '-' <|> Just <$> parseSquare
    halfMoveClock = decimal
    fullMoveNumber = decimal

pieceParser :: Parser Piece
pieceParser = choice $ map charMapping pieceChars

parseFenElement :: Parser FenElement
parseFenElement = NumberOfSquares <$> decimal <|> FenPiece <$> pieceParser

tagPair :: Parser (Text, Text)
tagPair = bracketed $ (,) <$> (Parser.takeWhile isLetter <* skipSpace) <*> quoted (Parser.takeWhile (/= '\"'))

quoted :: Parser a -> Parser a
quoted p = char '\"' *> p <* char '\"'

bracketed :: Parser a -> Parser a
bracketed p = char '[' *> p <* char ']'
