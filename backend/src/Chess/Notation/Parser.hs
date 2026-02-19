{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Chess.Notation.Parser (Parser, fen, fullMove, parseMove, parseMoveText, parseMoveWithAppendation, parseMoveWithAppendation2, parseMoveWithAppendation3, parseMoveWithAppendation', parsePgn, lexeme, parseMoveString) where

import Chess.Data hiding (move)
import Chess.Lib (withInput)
import Chess.Notation hiding (fen)
import Control.Applicative (many, optional, some, (<|>))
import Control.Monad.State (StateT (..), runStateT)
import Data.Attoparsec.Text as Parser (Parser, char, choice, decimal, endOfLine, skipSpace, takeWhile, takeWhile1, try)
import Data.Char (isLetter)
import Data.List (intersperse, singleton)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import Data.Void (Void)
import Debug.Trace (trace)

type Stream = Text

type Error = Void

pieceTypeMap :: Map.Map Char PieceType
pieceTypeMap = Map.fromList $ map swap pieceTypeChars

fileMap :: Map.Map Char File
fileMap = Map.fromList $ map swap fileChars

rankMap :: Map.Map Char Rank
rankMap = Map.fromList $ map swap rankChars

parseMoveText :: Parser MoveText
parseMoveText = MoveText <$> some (try fullMove) <*> result

result :: Parser (Maybe Result)
result = Just WinForWhite <$ "1-0" <|> Just WinForBlack <$ "0-1" <|> Just Draw <$ "1/2-1/2" <|> Nothing <$ "*"

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

parseMoveWithAppendation3 :: Parser MoveAndAppendation
parseMoveWithAppendation3 = MoveAndAppendation <$> parseMove3 <*> optional appendation

parseMoveWithAppendation2 :: Parser MoveAndAppendation
parseMoveWithAppendation2 = undefined

-- parseMoveWithAppendation2 = do
--  moveString <- parseMoveString
--  case runStateT parseMoveWithAppendation' moveString of
--    [(move, [])] -> pure move
--    _ -> unexpected $ Label ('e' :| ':' : moveString)

parseMoveWithAppendation' :: StateT String [] MoveAndAppendation
parseMoveWithAppendation' = MoveAndAppendation <$> parseMove' <*> optional parseAppendation' <* eol'
  where
    parseMove' = (RegularMove <$> notated) <|> (Castle <$> parseCastle')
    parseAppendation' = Check <$ char' '+' <|> Mate <$ char' '#'
    notated = NotatedMoveValues <$> pieceType' <*> fuzzySquare <*> parseMoveType' <*> square' <*> optional parsePromotion'
    fuzzySquare = (,) <$> optional file' <*> optional rank'
    parseMoveType' = Takes <$ char' 'x' <|> pure To
    square' = (,) <$> file' <*> rank'
    parsePromotion' = char' '=' *> pieceType'
    parseCastle' = Queenside <$ string' "O-O-O" <|> Kingside <$ string' "O-O"
    pieceType' = mapping' pieceTypeMap <|> pure Pawn
    file' = mapping' fileMap
    rank' = mapping' rankMap
    char' x = StateT f
      where
        f (a : s) | a == x = [(a, s)]
        f _ = []
    string' :: String -> StateT String [] String
    string' = traverse char'
    mapping' m' = StateT f
      where
        f (a : s) = maybe [] (singleton . (,s)) $ Map.lookup a m'
        f [] = []
    eol' = StateT f
      where
        f "" = [((), "")]
        f _ = []

parseMove :: Parser NotatedMove
parseMove = (RegularMove <$> move) <|> (Castle <$> parseCastle)
  where
    move = NotatedMoveValues <$> pieceType <*> fuzzySquare <*> parseMoveType <*> square <*> optional parsePromotion
    fuzzySquare = (,) <$> optional file <*> optional rank

parseMove3 :: Parser NotatedMove
parseMove3 = (RegularMove <$> (try simple <|> disambiguated)) <|> Castle <$> parseCastle
  where
    disambiguated = NotatedMoveValues <$> pieceType <*> fuzzySquare <*> parseMoveType <*> square <*> optional parsePromotion
    fuzzySquare = (,) <$> optional file <*> optional rank
    simple = move' <$> pieceType <*> parseMoveType <*> square <*> optional parsePromotion
      where
        move' notatedMovePiece moveType notatedToSquare notatedPromotion = NotatedMoveValues {notatedMovePiece, notatedFrom = (Nothing, Nothing), moveType, notatedToSquare, notatedPromotion}

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

square :: Parser Square
square = (,) <$> file <*> rank

pieceType :: Parser PieceType
pieceType = choice (map charMapping pieceTypeChars) <|> pure Pawn

parseDots :: Parser Dots
parseDots = lexeme (ThreeDots <$ "..." <|> OneDot <$ ".")

lexeme :: Parser a -> Parser a
lexeme p = p <* skipSpace

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

tagPair :: Parser (Text, Text)
tagPair = bracketed $ (,) <$> (Parser.takeWhile isLetter <* skipSpace) <*> quoted (Parser.takeWhile (/= '\"'))

quoted :: Parser a -> Parser a
quoted p = char '\"' *> p <* char '\"'

bracketed :: Parser a -> Parser a
bracketed p = char '[' *> p <* char ']'
