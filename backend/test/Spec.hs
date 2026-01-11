{-# LANGUAGE NamedFieldPuns #-}

import Chess.Data (File (..), Move (..), MoveType (..), PieceType (..), Rank (..))
import Chess.Game (Player (..), Result (..), fromMoves, play, startingGame)
import Chess.Notation
import Chess.Notation (NotatedMove (NotatedMove))
import Control.Monad.State
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (ParseErrorBundle, Parsec, eof, errorBundlePretty, parse, parseMaybe)

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ testShortGame,
        testFullMoveParser,
        testMoveParser,
        testMoveTextParser
      ]

testShortGame :: TestTree
testShortGame = testCase "shortGame" $ actual @?= expected
  where
    actual = fen <$> fromMoves shortGameMoves startingGame
    expected = Just shortGameFen
    shortGameMoves = concatMap flattenMove shortGameFullMoves
    shortGameFen = "rnbqkb1r/pppp1ppp/8/4P3/8/4n2P/PPPNPPP1/R1BQKBNR w KQkq - 1 5"

shortGameFullMoves :: [FullMove]
shortGameFullMoves =
  [ FullMove 1 (Just $ NotatedMove $ Move Pawn (Nothing, Nothing) To (D, Four)) (Just $ NotatedMove $ Move Knight (Nothing, Nothing) To (F, Six)),
    FullMove 2 (Just $ NotatedMove $ Move Knight (Nothing, Nothing) To (D, Two)) (Just $ NotatedMove $ Move Pawn (Nothing, Nothing) To (E, Five)),
    FullMove 3 (Just $ NotatedMove $ Move Pawn (Just D, Nothing) Takes (E, Five)) (Just $ NotatedMove $ Move Knight (Nothing, Nothing) To (G, Four)),
    FullMove 4 (Just $ NotatedMove $ Move Pawn (Nothing, Nothing) To (H, Three)) (Just $ NotatedMove $ Move Knight (Nothing, Nothing) To (E, Three))
  ]

testMoveTextParser :: TestTree
testMoveTextParser = testCase "Movetext" $ actual @?= expected
  where
    actual = parseShouldSucceed moveText shortGamePgn
    expected = MoveText shortGameFullMoves expectedResult
    expectedResult = Just WinForWhite

parseShouldSucceed ::
  Parser a ->
  String ->
  a
parseShouldSucceed p =
  either (error . errorBundlePretty) id . parse (p <* eof) "<test>"

shortGamePgn :: String
shortGamePgn = "1.d4 Nf6 2.Nd2 e5 3.dxe5 Ng4 4.h3 Ne3 1-0"

testFullMoveParser :: TestTree
testFullMoveParser = testCase "fullMove" $ actual @?= expected
  where
    expected = FullMove 1 (Just whiteMove) (Just blackMove)
    whiteMove = NotatedMove $ Move Pawn (Nothing, Nothing) To (D, Four)
    blackMove = NotatedMove $ Move Knight (Nothing, Nothing) To (F, Six)
    actual = parseShouldSucceed fullMove "1.d4 Nf6 "

testMoveParser :: TestTree
testMoveParser = testGroup "test move" $ map moveTest moveCases
  where
    moveCases =
      [ ("d4", Move Pawn (Nothing, Nothing) To (D, Four)),
        ("Nf6", Move Knight (Nothing, Nothing) To (F, Six)),
        ("dxe5", Move Pawn (Just D, Nothing) Takes (E, Five)),
        ("Rdf8", Move Rook (Just D, Nothing) To (F, Eight)),
        ("R1a3", Move Rook (Nothing, Just One) To (A, Three)),
        ("Qh4e1", Move Queen (Just H, Just Four) To (E, One))
      ]
    moveTest (string, expected) = testCase string $ actual @?= NotatedMove expected
      where
        actual = parseShouldSucceed move string
