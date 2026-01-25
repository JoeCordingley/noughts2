{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Chess.Game (play, GetAction, Board, Result (..), Player (..), Square, Rank (..), File (..), Move (..), legalMoves, PieceType (..), ranks, files, startingBoard, fromMoves, Action (..), startingGame) where

import Chess.Data
import Chess.Lib (singletonMaybe)
import Control.Monad (guard)
import Data.Functor.Identity (Identity (..))
import Data.List (singleton)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (isNothing, maybeToList)
import Data.Semigroup (First (..))
import qualified Data.Set as Set

data Result
  = Winner Player
  | Draw
  deriving (Eq, Show)

data Status
  = Playing
  | Finished Result

data EndStatus
  = Checkmate
  | Stalemate

data Action = MoveAction FullyDefinedMove | Resign

play :: (Monad f) => GetAction f -> f (Result, Game)
play getAction = play' startingGame
  where
    play' game = do
      (status, newGame) <- playMove getAction game
      case status of
        Playing -> play' newGame
        Finished result -> return (result, newGame)

startingBoard :: Board
startingBoard =
  Map.fromList $
    [ ((file, rank), (player, pieceType))
      | (rank, player, pieceTypes) <-
          [ (One, White, otherPieces),
            (Two, White, pawns),
            (Seven, Black, pawns),
            (Eight, Black, otherPieces)
          ],
        (file, pieceType) <- files `zip` pieceTypes
    ]
  where
    pawns = replicate 8 Pawn
    otherPieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

playMove :: (Functor f) => GetAction f -> PlayMove f
playMove getAction game = g <$> getAction game
  where
    g (MoveAction move) = f $ applyMove game move
    g Resign = (statusResigned, game) where statusResigned = Finished . Winner . other $ playerToMove game
    f newGame = (postMoveStatus newGame, newGame)

disambiguate :: Game -> AmbiguousMove -> [FullyDefinedMove]
disambiguate game move = filter (matches move) $ legalMoves game
  where
    matches (Move p1 (maybeOriginFile, maybeOriginRank) x1 d1 _) (Move p2 (of2, or2) x2 d2 _) =
      p1 == p2
        && x1 == x2
        && d1 == d2
        && all (== of2) maybeOriginFile
        && all (== or2) maybeOriginRank

type GetAction f = Game -> f Action

startingGame :: Game
startingGame = Game {gameBoard = startingBoard, playerToMove = White, castlesAvailable = Set.fromList allCastles, enPassantSquare = Nothing, halfMoveClock = 0, fullMoveNumber = 1}

fromMoves :: [AmbiguousMove] -> Game -> Maybe Game
fromMoves = foldr f pure
  where
    f move g game = singletonMaybe (disambiguate game move) >>= g . applyMove game

type PlayMove f = Game -> f (Status, Game)

finishedStatus :: Game -> Maybe EndStatus
finishedStatus game = if null $ legalMoves game then Just (if kingIsUnderAttack then Checkmate else Stalemate) else Nothing
  where
    kingIsUnderAttack = King `elem` piecesUnderAttack
    piecesUnderAttack = [snd piece | move <- attackingMoves (playerToMove game) (gameBoard game), piece <- maybeToList $ pieceUnderAttack move]
    pieceUnderAttack = pieceAt (gameBoard game) . toSquare

result :: Player -> EndStatus -> Result
result player Checkmate = Winner player
result _ Stalemate = Draw

postMoveStatus :: Game -> Status
postMoveStatus game = maybe Playing (Finished . result (playerToMove game)) $ finishedStatus game

legalMoves :: Game -> [FullyDefinedMove]
legalMoves game = filter legal $ attackingMoves player board' ++ nonAttackingMoves player board'
  where
    legal move = not . isUnderCheck player $ applyMoveToBoard move board'
    player = playerToMove game
    board' = gameBoard game

nonAttackingMoves :: Player -> Board -> [FullyDefinedMove]
nonAttackingMoves player board = do
  (movePiece, fromSquare) <- piecePositions player board
  lineOfMovement <- linesOfMovement (player, movePiece) fromSquare
  toSquare <- takeWhile unoccupied lineOfMovement
  return $ Move movePiece fromSquare To toSquare Nothing
  where
    -- TODO: checkType

    unoccupied space = isNothing $ pieceAt board space

isUnderCheck :: Player -> Board -> Bool
isUnderCheck player board = King `elem` piecesUnderAttack
  where
    piecesUnderAttack = [snd piece | move <- attackingMoves (other player) board, piece <- maybeToList $ pieceUnderAttack move]
    pieceUnderAttack = pieceAt board . toSquare

linesOfMovement :: Piece -> Square -> [[Square]]
linesOfMovement (player, pieceType) = case pieceType of
  Pawn -> singleton . pawnMoves player
  Knight -> map singleton . knightMoves
  Bishop -> bishopLines
  Rook -> rookLines
  King -> map singleton . kingMoves
  Queen -> queenLines

attackingMoves :: Player -> Board -> [FullyDefinedMove]
attackingMoves player board = do
  (movePiece, fromSquare) <- piecePositions player board
  spaces <- linesOfAttack (player, movePiece) fromSquare
  maybeToList $ do
    (toSquare, (ownerOfPiece, _)) <- firstJust (fmapSnd $ pieceAt board) spaces
    guard $ ownerOfPiece == other player
    return $ Move movePiece fromSquare Takes toSquare Nothing
  where
    fmapSnd f a = (,) a <$> f a

linesOfAttack :: Piece -> Square -> [[Square]]
linesOfAttack (player, Pawn) = map singleton . pawnAttacks player
linesOfAttack piece = linesOfMovement piece

pawnMoves :: Player -> Square -> [Square]
pawnMoves = traverse . pawnRanks
  where
    pawnRanks White Two = [Three, Four]
    pawnRanks White rank = [succ rank]
    pawnRanks Black Seven = [Six, Five]
    pawnRanks Black rank = [pred rank]

pawnAttacks :: Player -> Square -> [Square]
pawnAttacks player = pawnAttacks'
  where
    pawnAttacks' (A, rank) = [(B, advanceOne rank)]
    pawnAttacks' (H, rank) = [(G, advanceOne rank)]
    pawnAttacks' (file, rank) = map (,advanceOne rank) [pred file, succ file]
    advanceOne rank = case player of
      White -> succ rank
      Black -> pred rank

knightMoves :: Square -> [Square]
knightMoves space = filter (knightMove space) allSpaces
  where
    knightMove x y = distances' == (1, 2) || distances' == (2, 1)
      where
        distances' = distances x y

distances ::
  (Enum a, Enum b) =>
  (a, b) ->
  (a, b) ->
  (Int, Int)
distances (a1, b1) (a2, b2) = (a1 `distance` a2, b1 `distance` b2)
  where
    distance x y = abs $ fromEnum x - fromEnum y

bishopLines :: Square -> [[Square]]
bishopLines space = map (lineExtendingFrom space) diagonalDirections
  where
    diagonalDirections = (,) <$> [-1, 1] <*> [-1, 1]

queenLines :: Square -> [[Square]]
queenLines space = bishopLines space ++ rookLines space

rookLines :: Square -> [[Square]]
rookLines space = map (lineExtendingFrom space) orthogonalDirections
  where
    orthogonalDirections = map (,0) [-1, 1] ++ map (0,) [-1, 1]

kingMoves :: Square -> [Square]
kingMoves space = filter (kingMove space) allSpaces
  where
    kingMove x y = uncurry max (distances x y) == 1

lineExtendingFrom :: Square -> (Int, Int) -> [Square]
lineExtendingFrom (file, rank) (fileIncrement, rankIncrement) =
  enumFromByIncrement file fileIncrement
    `zip` enumFromByIncrement rank rankIncrement

enumFromByIncrement :: (Enum a, Bounded a) => a -> Int -> [a]
enumFromByIncrement a inc = map toEnum [init, init + inc .. fromEnum $ maxBound `asTypeOf` a]
  where
    init = fromEnum a

allSpaces :: [Square]
allSpaces = (,) <$> files <*> ranks

piecePositions :: Player -> Board -> [(PieceType, Square)]
piecePositions player board = do
  space <- allSpaces
  (player', pieceType) <- maybeToList $ Map.lookup space board
  guard $ player' == player
  return (pieceType, space)

other :: Player -> Player
other White = Black
other Black = White

pieceAt :: Board -> Square -> Maybe Piece
pieceAt board space = Map.lookup space board

applyMoveToBoard :: FullyDefinedMove -> Board -> Board
applyMoveToBoard move board = Map.insert (toSquare move) (board ! from) (Map.delete from board)
  where
    from = fromSquare move

applyMove :: Game -> FullyDefinedMove -> Game
applyMove (Game {gameBoard, playerToMove, castlesAvailable, halfMoveClock, fullMoveNumber}) move = Game {gameBoard = applyMoveToBoard move gameBoard, playerToMove = other player, castlesAvailable, enPassantSquare = enPassantTarget move, halfMoveClock = if halfMoveClockResets then 0 else halfMoveClock + 1, fullMoveNumber = if playerToMove == Black then fullMoveNumber + 1 else fullMoveNumber}
  where
    player = playerToMove
    halfMoveClockResets = isCapture move || movePiece move == Pawn

isCapture :: Move f -> Bool
isCapture move = moveType move == Takes

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = fmap getFirst . foldMap (fmap First . f)

enPassantTarget :: FullyDefinedMove -> Maybe Square
enPassantTarget (Move {movePiece, fromSquare, toSquare}) = case (movePiece, fromRank, toRank) of
  (Pawn, Two, Four) -> Just (file, Three)
  (Pawn, Seven, Five) -> Just (file, Six)
  _ -> Nothing
  where
    (file, fromRank) = fromSquare
    (_, toRank) = toSquare
