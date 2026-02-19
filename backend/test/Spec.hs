{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Chess.Data
import Chess.Game (gameCheckType, legalMoves, startingGame)
import Chess.Notation as Notation
import Chess.Notation.Parser as Parser
import Control.Applicative (many)
import Control.Monad (void)
import Control.Monad.State (runStateT)
import Data.Attoparsec.Text (IResult (..), endOfInput, parseOnly)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Text.IO as T
import Test.Tasty
import Test.Tasty.HUnit

readPgns :: IO [(String, Text)]
readPgns = traverse readPgn ["Mamedyarov", "OneGame"]
  where
    readPgn name = (name,) <$> T.readFile ("pgns/" <> name <> ".pgn")

-- import Test.Hspec
-- import System.IO
--
-- main :: IO ()
-- main = hspec $ do
--  describe "File reading" $ do
--    it "reads contents from a file" $ do
--      writeFile "testfile.txt" "Hello, Haskell!"
--      contents <- readFile "testfile.txt"
--      contents `shouldBe` "Hello, Haskell!"
--      -- Clean up
--      removeFile "testfile.txt"

main :: IO ()
main = do
  pgns <- readPgns
  defaultMain $
    testGroup
      "All Tests"
      [ testOneMove,
        testFullMoveParser,
        testMoveParser,
        -- testMoveParser2,
        -- testMoveTextParser
        -- testFenParser,
        -- testLegalMoves,
        -- testShortGame,
        testReadPgns pgns
      ]

testOneMove :: TestTree
testOneMove = testCase "oneMove" $ actual @?= expected
  where
    actual = Notation.fen <$> fromMoves [MoveAndAppendation (notatedMove Pawn (Nothing, Nothing) To (E, Four)) Nothing] startingGame
    expected = Just shortGameFen
    shortGameFen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

shortGameFullMoves :: [FullMove]
shortGameFullMoves =
  [ FullMove 1 (Just $ MoveAndAppendation (notatedMove Pawn (Nothing, Nothing) To (D, Four)) Nothing) (Just $ MoveAndAppendation (notatedMove Knight (Nothing, Nothing) To (F, Six)) Nothing),
    FullMove 2 (Just $ MoveAndAppendation (notatedMove Knight (Nothing, Nothing) To (D, Two)) Nothing) (Just $ MoveAndAppendation (notatedMove Pawn (Nothing, Nothing) To (E, Five)) Nothing),
    FullMove 3 (Just $ MoveAndAppendation (notatedMove Pawn (Just D, Nothing) Takes (E, Five)) Nothing) (Just $ MoveAndAppendation (notatedMove Knight (Nothing, Nothing) To (G, Four)) Nothing),
    FullMove 4 (Just $ MoveAndAppendation (notatedMove Pawn (Nothing, Nothing) To (H, Three)) Nothing) Nothing
  ]

testMoveTextParser :: TestTree
testMoveTextParser = testCase "Movetext" $ actual @?= expected
  where
    actual = parseShouldSucceed parseMoveText shortGamePgn
    expected = MoveText shortGameFullMoves expectedResult
    expectedResult = Just WinForWhite

parseShouldSucceed ::
  (Show a) =>
  Parser a ->
  Text ->
  a
parseShouldSucceed p =
  either (error . show) id . parseOnly (p <* endOfInput)

shortGamePgn :: Text
shortGamePgn = "1.d4 Nf6 2.Nd2 e5 3.dxe5 Ng4 4.h3 1-0"

testFullMoveParser :: TestTree
testFullMoveParser = testGroup "fullMoves" $ [fullCase, halfCase]
  where
    fullCase = testCase "full case" $ actual @?= expected
      where
        expected = FullMove 1 (Just whiteMove) (Just blackMove)
        whiteMove = MoveAndAppendation (notatedMove Pawn (Nothing, Nothing) To (D, Four)) Nothing
        blackMove = MoveAndAppendation (notatedMove Knight (Nothing, Nothing) To (F, Six)) Nothing
        actual = parseShouldSucceed fullMove "1.d4 Nf6 "
    halfCase = testCase "half case" $ actual @?= expected
      where
        expected = FullMove 39 (Just whiteMove) Nothing
        whiteMove = MoveAndAppendation (notatedMove Pawn (Nothing, Nothing) To (H, Four)) (Just Check)
        actual = parseShouldSucceed fullMove "39.h4+"

testMoveParser :: TestTree
testMoveParser = testGroup "test move" $ map moveTest moveCases
  where
    moveCases =
      [ ("d4", MoveAndAppendation (notatedMove Pawn (Nothing, Nothing) To (D, Four)) Nothing),
        ("Nf6", MoveAndAppendation (notatedMove Knight (Nothing, Nothing) To (F, Six)) Nothing),
        ("dxe5", MoveAndAppendation (notatedMove Pawn (Just D, Nothing) Takes (E, Five)) Nothing),
        ("Rdf8", MoveAndAppendation (notatedMove Rook (Just D, Nothing) To (F, Eight)) Nothing),
        ("R1a3", MoveAndAppendation (notatedMove Rook (Nothing, Just One) To (A, Three)) Nothing),
        ("Qh4e1", MoveAndAppendation (notatedMove Queen (Just H, Just Four) To (E, One)) Nothing),
        ("R1a3+", MoveAndAppendation (notatedMove Rook (Nothing, Just One) To (A, Three)) (Just Check)),
        ("R1a3#", MoveAndAppendation (notatedMove Rook (Nothing, Just One) To (A, Three)) (Just Mate))
      ]
    moveTest (string, expected) = testCase string $ actual @?= expected
      where
        actual = parseShouldSucceed parseMoveWithAppendation $ pack string

testMoveParser2 :: TestTree
testMoveParser2 = testGroup "test move" $ map moveTest moveCases
  where
    moveCases =
      [ ("d4", MoveAndAppendation (notatedMove Pawn (Nothing, Nothing) To (D, Four)) Nothing),
        ("Nf6", MoveAndAppendation (notatedMove Knight (Nothing, Nothing) To (F, Six)) Nothing),
        ("dxe5", MoveAndAppendation (notatedMove Pawn (Just D, Nothing) Takes (E, Five)) Nothing),
        ("Rdf8", MoveAndAppendation (notatedMove Rook (Just D, Nothing) To (F, Eight)) Nothing),
        ("R1a3", MoveAndAppendation (notatedMove Rook (Nothing, Just One) To (A, Three)) Nothing),
        ("Qh4e1", MoveAndAppendation (notatedMove Queen (Just H, Just Four) To (E, One)) Nothing),
        ("R1a3+", MoveAndAppendation (notatedMove Rook (Nothing, Just One) To (A, Three)) (Just Check)),
        ("R1a3#", MoveAndAppendation (notatedMove Rook (Nothing, Just One) To (A, Three)) (Just Mate)),
        ("Rxh1+", MoveAndAppendation (notatedMove Rook (Nothing, Nothing) Takes (H, One)) (Just Check)),
        ("h4+", MoveAndAppendation (notatedMove Pawn (Nothing, Nothing) To (H, Four)) (Just Check))
      ]
    moveTest (string, expected) = testCase string $ actual @?= [(expected, [])]
      where
        actual = runStateT parseMoveWithAppendation' $ string

testFenParser :: TestTree
testFenParser = testGroup "fenParser" [testCase "openingBoard" $ actual @?= expected]
  where
    expected = startingGame
    actual = parseShouldSucceed Parser.fen $ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

testLegalMoves :: TestTree
testLegalMoves = testGroup "legalMoves" [testCase "openingBoard" $ Set.fromList actual @?= Set.fromList expected]
  where
    actual = Notation.notateMoves . map (fmap gameCheckType) $ legalMoves startingGame
    expected = ["a3", "a4", "b3", "b4", "c3", "c4", "d3", "d4", "e3", "e4", "f3", "f4", "g3", "g4", "h3", "h4", "Na3", "Nc3", "Nf3", "Nh3"]

testShortGame :: TestTree
testShortGame = testCase "shortGame" $ actual @?= expected
  where
    actual = Notation.fen <$> fromMoves shortGameMoves startingGame
    expected = Just shortGameFen
    shortGameMoves = concatMap flattenMove shortGameFullMoves
    shortGameFen = "rnbqkb1r/pppp1ppp/8/4P3/8/4n2P/PPPNPPP1/R1BQKBNR w KQkq - 1 5"

testReadPgns :: [(String, Text)] -> TestTree
testReadPgns = testGroup "readPgns" . map testReadPgnCase
  where
    testReadPgnCase (name, fileText) = testCase name $ actual @?= ()
      where
        actual = parseShouldSucceed (void $ Parser.lexeme $ many $ Parser.parsePgn) fileText
