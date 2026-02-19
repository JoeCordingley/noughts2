{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Chess.Notation.Parser (Parser, fen, fullMove, parseMove, parseMoveText, parseMoveWithAppendation, parseMoveWithAppendation2, parseMoveWithAppendation', parsePgn, lexeme, parseMoveText, parseMoveString) where

import Chess.Data hiding (move)
import Chess.Lib (withInput)
import Chess.Notation hiding (fen)
import Control.Applicative (optional, (<|>))
import Control.Monad.State (StateT (..), runStateT)
import Data.Char (isLetter)
import Data.List (intersperse, singleton)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (ErrorItem (..), MonadParsec (try), Parsec, Tokens, choice, many, some, takeWhile1P, takeWhileP, unexpected)
import Text.Megaparsec.Char (char, eol, hspace, letterChar, newline, space)
import Text.Megaparsec.Char.Lexer (decimal)

type Stream = Text

type Error = Void

type Parser a = Parsec Error Stream a

parseMoveText :: Parser MoveText
parseMoveText = MoveText <$> some (try fullMove) <*> result

result :: Parser (Maybe Result)
result = Just WinForWhite <$ "1-0" <|> Just WinForBlack <$ "0-1" <|> Just Draw <$ "1/2-1/2" <|> Nothing <$ "*"

fullMove :: Parser FullMove
fullMove = do
  n <- decimal
  dots <- parseDots
  white <- case dots of
    OneDot -> Just <$> lexeme parseMoveWithAppendation2
    ThreeDots -> pure Nothing
  black <- optional . try $ lexeme parseMoveWithAppendation2
  return $ FullMove n white black

parseMoveWithAppendation :: Parser MoveAndAppendation
parseMoveWithAppendation = MoveAndAppendation <$> parseMove <*> optional appendation

parseMoveWithAppendation2 :: Parser MoveAndAppendation
parseMoveWithAppendation2 = do
  moveString <- parseMoveString
  case runStateT parseMoveWithAppendation' moveString of
    [(move, [])] -> pure move
    _ -> unexpected $ Label ('e' :| ':' : moveString)

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
    pieceType' = mapping' pieceTypeChars <|> pure Pawn
    file' = mapping' fileChars
    rank' = mapping' rankChars
    char' x = StateT f
      where
        f (a : s) | a == x = [(a, s)]
        f _ = []
    string' :: String -> StateT String [] String
    string' = traverse char'
    mapping' m = StateT f
      where
        m' = Map.fromList $ map swap m
        f (a : s) = maybe [] (singleton . (,s)) $ Map.lookup a m'
        f [] = []
    eol' = StateT f
      where
        f "" = [((), "")]
        f _ = []

parseMove :: Parser NotatedMove
parseMove = (RegularMove <$> (try disambiguated <|> simple)) <|> Castle <$> parseCastle
  where
    disambiguated = NotatedMoveValues <$> pieceType <*> fuzzySquare <*> parseMoveType <*> square <*> optional parsePromotion
    fuzzySquare = (,) <$> optional file <*> optional rank
    simple = move' <$> pieceType <*> parseMoveType <*> square <*> optional parsePromotion
      where
        move' notatedMovePiece moveType notatedToSquare notatedPromotion = NotatedMoveValues {notatedMovePiece, notatedFrom = (Nothing, Nothing), moveType, notatedToSquare, notatedPromotion}

parseMoveString :: Parser String
parseMoveString = unpack <$> takeWhile1P (Just "move characters") (`Set.member` moveChars)
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
lexeme p = p <* space

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

parsePgn :: Parser PGN
parsePgn = PGN <$> many (tagPair <* eol) <* space <*> lexeme parseMoveText

tagPair :: Parser (Text, Text)
tagPair = bracketed $ (,) <$> (takeWhile1P Nothing isLetter <* hspace) <*> quoted (takeWhileP Nothing (/= '\"'))

quoted :: Parser a -> Parser a
quoted p = char '\"' *> p <* char '\"'

bracketed :: Parser a -> Parser a
bracketed p = char '[' *> p <* char ']'
