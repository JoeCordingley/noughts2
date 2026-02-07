import Chess.Data
import Chess.Game (applyMoveToBoard, attackingMoves, fromMoves, legalMoves, simpleMoves, startingBoard, startingBoardStatus, startingGame, updateBoardStatus)
import Chess.Notation as Notation
import Chess.Notation.Parser as Parser
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (eof, errorBundlePretty, parse)

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ testOneMove,
        testFullMoveParser,
        testMoveParser,
        testMoveTextParser,
        testFenParser,
        -- testLegalMoves,
        testAttackingMoves,
        testSimpleMoves,
        testShortGame
      ]

testOneMove :: TestTree
testOneMove = testCase "oneMove" $ actual @?= expected
  where
    actual = Notation.fen <$> fromMoves [NotatedMove Pawn (Nothing, Nothing) To (E, Four) Nothing] startingGame
    expected = Just shortGameFen
    shortGameFen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

shortGameFullMoves :: [FullMove]
shortGameFullMoves =
  [ FullMove 1 (Just $ NotatedMove Pawn (Nothing, Nothing) To (D, Four) Nothing) (Just $ NotatedMove Knight (Nothing, Nothing) To (F, Six) Nothing),
    FullMove 2 (Just $ NotatedMove Knight (Nothing, Nothing) To (D, Two) Nothing) (Just $ NotatedMove Pawn (Nothing, Nothing) To (E, Five) Nothing),
    FullMove 3 (Just $ NotatedMove Pawn (Just D, Nothing) Takes (E, Five) Nothing) (Just $ NotatedMove Knight (Nothing, Nothing) To (G, Four) Nothing),
    FullMove 4 (Just $ NotatedMove Pawn (Nothing, Nothing) To (H, Three) Nothing) (Just $ NotatedMove Knight (Nothing, Nothing) To (E, Three) Nothing)
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
    whiteMove = NotatedMove Pawn (Nothing, Nothing) To (D, Four) Nothing
    blackMove = NotatedMove Knight (Nothing, Nothing) To (F, Six) Nothing
    actual = parseShouldSucceed fullMove "1.d4 Nf6 "

testMoveParser :: TestTree
testMoveParser = testGroup "test move" $ map moveTest moveCases
  where
    moveCases =
      [ ("d4", NotatedMove Pawn (Nothing, Nothing) To (D, Four) Nothing),
        ("Nf6", NotatedMove Knight (Nothing, Nothing) To (F, Six) Nothing),
        ("dxe5", NotatedMove Pawn (Just D, Nothing) Takes (E, Five) Nothing),
        ("Rdf8", NotatedMove Rook (Just D, Nothing) To (F, Eight) Nothing),
        ("R1a3", NotatedMove Rook (Nothing, Just One) To (A, Three) Nothing),
        ("Qh4e1", NotatedMove Queen (Just H, Just Four) To (E, One) Nothing),
        ("R1a3+", NotatedMove Rook (Nothing, Just One) To (A, Three) (Just Check)),
        ("R1a3#", NotatedMove Rook (Nothing, Just One) To (A, Three) (Just Mate))
      ]
    moveTest (string, expected) = testCase string $ actual @?= expected
      where
        actual = parseShouldSucceed parseMove string

testFenParser :: TestTree
testFenParser = testGroup "fenParser" [testCase "openingBoard" $ actual @?= expected]
  where
    expected = startingGame
    actual = parseShouldSucceed Parser.fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

-- testLegalMoves :: TestTree
-- testLegalMoves = testGroup "legalMoves" [testCase "openingBoard" $ take 3 actual @?= []]
--  where
--    actual = legalMoves White startingBoardStatus

testAttackingMoves :: TestTree
testAttackingMoves = testGroup "attackingMoves" $ [testCase "openingBoard" $ actual @?= expected]
  where
    actual = attackingMoves White startingBoard
    expected = []

testSimpleMoves :: TestTree
testSimpleMoves = testGroup "simpleMoves" $ [testCase "openingBoard" $ Set.fromList actual @?= Set.fromList expected]
  where
    actual = simpleMoves White startingBoard
    expected = pawnMoves <> knightMoves
      where
        pawnMoves = [Movement (Pawn, (file, Two)) (file, rank) | file <- files, rank <- [Three, Four]]
        knightMoves = [Movement (Knight, (fromFile, One)) (toFile, Three) | (fromFile, toFile) <- [(B, A), (B, C), (G, F), (G, H)]]

testShortGame :: TestTree
testShortGame = testCase "shortGame" $ actual @?= expected
  where
    actual = Notation.fen <$> fromMoves shortGameMoves startingGame
    expected = Just shortGameFen
    shortGameMoves = concatMap flattenMove shortGameFullMoves
    shortGameFen = "rnbqkb1r/pppp1ppp/8/4P3/8/4n2P/PPPNPPP1/R1BQKBNR w KQkq - 1 5"
