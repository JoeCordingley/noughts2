{-# LANGUAGE NamedFieldPuns #-}

import Chess.Data (File (..), PieceType (..), Rank (..))
import Chess.Game (Player (..), Result (..), fromMoves, play, playMove)
import Chess.Notation
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
    actual = andFen <$> evalStateT (play $ playMove fromMoves) shortGameMoves
      where
        andFen (status, board) = (status, fen board)
    expected = Just (Winner Black, shortGameFen)
    shortGameMoves = concatMap flattenMove shortGameFullMoves
    shortGameFen = "rnbqkb1r/pppp1ppp/8/4P3/8/4n2P/PPPNPPP1/R1BQKBNR w KQkq - 1 5"

baseMove = Move {fromRank = Nothing, fromFile = Nothing, isCapture = False}

shortGameFullMoves :: [FullMove]
shortGameFullMoves =
  [ FullMove 1 (Just baseMove {movePiece = Pawn, toSpace = (D, Four)}) (Just baseMove {movePiece = Knight, toSpace = (F, Six)}),
    FullMove 2 (Just baseMove {movePiece = Knight, toSpace = (D, Two)}) (Just baseMove {movePiece = Pawn, toSpace = (E, Five)}),
    FullMove 3 (Just baseMove {movePiece = Pawn, fromFile = Just D, isCapture = True, toSpace = (E, Five)}) (Just baseMove {movePiece = Knight, toSpace = (G, Four)}),
    FullMove 4 (Just baseMove {movePiece = Pawn, toSpace = (H, Three)}) (Just baseMove {movePiece = Knight, toSpace = (E, Three)})
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
    whiteMove = baseMove {movePiece = Pawn, toSpace = (D, Four)}
    blackMove = baseMove {movePiece = Knight, toSpace = (F, Six)}
    actual = parseShouldSucceed fullMove "1.d4 Nf6 "

testMoveParser :: TestTree
testMoveParser = testGroup "test move" $ map moveTest moveCases
  where
    moveCases =
      [ ("d4", baseMove {movePiece = Pawn, toSpace = (D, Four)}),
        ("Nf6", baseMove {movePiece = Knight, toSpace = (F, Six)}),
        ("dxe5", baseMove {movePiece = Pawn, fromFile = Just D, toSpace = (E, Five), isCapture = True}),
        ("Rdf8", baseMove {movePiece = Rook, fromFile = Just D, toSpace = (F, Eight)}),
        ("R1a3", baseMove {movePiece = Rook, fromRank = Just One, toSpace = (A, Three)}),
        ("Qh4e1", baseMove {movePiece = Queen, fromFile = Just H, fromRank = Just Four, toSpace = (E, One)})
      ]
    moveTest (string, expected) = testCase string $ actual @?= expected
      where
        actual = parseShouldSucceed move string

-- moveText :: Move -> String
-- moveText (Move {movePiece, fromRow, fromColumn, isCapture, toSpace}) =
