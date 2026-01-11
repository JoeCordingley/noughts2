{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Chess.Game (play, GetAction, Board, Result (..), Player (..), Space, Rank (..), File (..), Move (..), legalMoves, PieceType (..), ranks, files, startingBoard, playMove, fromMoves, Action (..)) where

import Chess.Data
import Chess.Lib (singletonMaybe)
import qualified Chess.Notation as Notation (Move (..))
import Control.Monad (guard)
import Control.Monad.State (StateT (..), state)
import Data.List (singleton)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (isNothing, maybeToList)
import Data.Semigroup (First (..))

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

data Move
  = Move {movePiece :: PieceType, fromSpace :: Space, toSpace :: Space, isCapture :: Bool}
  deriving (Eq)

data Action = MoveAction Move | Resign

data AttackingMove = AttackingMove {attackingPiece :: PieceType, pieceUnderAttack :: PieceType, attackingFrom :: Space, attackingTo :: Space}

play :: (Monad f) => PlayMove f -> f (Result, Board)
play playMove = play' White startingBoard
  where
    play' player board = do
      (status, newBoard) <- playMove player board
      case status of
        Playing -> play' (nextPlayer player) newBoard
        Finished result -> return (result, newBoard)
    nextPlayer = other

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
playMove getMove player board = g <$> getMove player board
  where
    g (MoveAction move) = f $ applyMove board move
    g Resign = (statusResigned, board) where statusResigned = Finished . Winner $ other player
    f newBoard = (postMoveStatus player newBoard, newBoard)

disambiguate :: Player -> Board -> Notation.Move -> [Move]
disambiguate player board writtenMove = filter (isMove writtenMove) $ legalMoves player board
  where
    isMove (Notation.Move pieceType maybeOriginFile maybeOriginRank isCapture' destination) move =
      movePiece move
        == pieceType
        && isCapture'
          == isCapture move
        && toSpace move
          == destination
        && all (== (fst . fromSpace $ move)) maybeOriginFile
        && all (== (snd . fromSpace $ move)) maybeOriginRank

type GetAction f = Player -> Board -> f Action

fromMoves :: GetAction (StateT [Notation.Move] Maybe)
fromMoves player board = StateT f
  where
    f (move : moves) = (,moves) . MoveAction <$> (singletonMaybe $ disambiguate player board move)
    f [] = Just (Resign, [])

type PlayMove f = Player -> Board -> f (Status, Board)

finishedStatus :: Player -> Board -> Maybe EndStatus
finishedStatus player board = if null $ legalMoves (other player) board then Just (if kingIsUnderAttack then Checkmate else Stalemate) else Nothing
  where
    kingIsUnderAttack = King `elem` piecesUnderAttack
    piecesUnderAttack = pieceUnderAttack <$> attackingMoves player board

result :: Player -> EndStatus -> Result
result player Checkmate = Winner player
result _ Stalemate = Draw

postMoveStatus :: Player -> Board -> Status
postMoveStatus player board = maybe Playing (Finished . result player) $ finishedStatus player board

legalMoves :: Player -> Board -> [Move]
legalMoves player board = (map toMove (attackingMoves player board) ++ nonAttackingMoves player board)
  where
    toMove attackingMove = Move {movePiece = attackingPiece attackingMove, fromSpace = attackingFrom attackingMove, toSpace = attackingTo attackingMove, isCapture = True}

nonAttackingMoves :: Player -> Board -> [Move]
nonAttackingMoves player board = do
  (pieceType, fromSpace) <- piecePositions player board
  lineOfMovement <- linesOfMovement (player, pieceType) fromSpace
  toSpace <- takeWhile unoccupied lineOfMovement
  return $ Move {movePiece = pieceType, fromSpace, toSpace, isCapture = False}
  where
    unoccupied space = isNothing $ pieceAt board space

linesOfMovement :: Piece -> Space -> [[Space]]
linesOfMovement (player, pieceType) = case pieceType of
  Pawn -> singleton . pawnMoves player
  Knight -> map singleton . knightMoves
  Bishop -> bishopLines
  Rook -> rookLines
  King -> map singleton . kingMoves
  Queen -> queenLines

attackingMoves :: Player -> Board -> [AttackingMove]
attackingMoves player board = do
  (pieceType, fromSpace) <- piecePositions player board
  spaces <- linesOfAttack (player, pieceType) fromSpace
  maybeToList $ do
    (toSpace, piece) <- firstJust (fmapSnd $ pieceAt board) spaces
    attacking <- enemyPiece player piece
    return $ AttackingMove {attackingPiece = pieceType, pieceUnderAttack = attacking, attackingFrom = fromSpace, attackingTo = toSpace}
  where
    fmapSnd f a = (,) a <$> f a

linesOfAttack :: Piece -> Space -> [[Space]]
linesOfAttack (player, Pawn) = map singleton . pawnAttacks player
linesOfAttack piece = linesOfMovement piece

pawnMoves :: Player -> Space -> [Space]
pawnMoves = traverse . pawnRanks
  where
    pawnRanks White Two = [Three, Four]
    pawnRanks White rank = [succ rank]
    pawnRanks Black Seven = [Six, Five]
    pawnRanks Black rank = [pred rank]

pawnAttacks :: Player -> Space -> [Space]
pawnAttacks player = pawnAttacks'
  where
    pawnAttacks' (A, rank) = [(B, advanceOne rank)]
    pawnAttacks' (H, rank) = [(G, advanceOne rank)]
    pawnAttacks' (file, rank) = map (,advanceOne rank) [pred file, succ file]
    advanceOne rank = case player of
      White -> succ rank
      Black -> pred rank

knightMoves :: Space -> [Space]
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

bishopLines :: Space -> [[Space]]
bishopLines space = map (lineExtendingFrom space) diagonalDirections
  where
    diagonalDirections = (,) <$> [-1, 1] <*> [-1, 1]

queenLines :: Space -> [[Space]]
queenLines space = bishopLines space ++ rookLines space

rookLines :: Space -> [[Space]]
rookLines space = map (lineExtendingFrom space) orthogonalDirections
  where
    orthogonalDirections = map (,0) [-1, 1] ++ map (0,) [-1, 1]

kingMoves :: Space -> [Space]
kingMoves space = filter (kingMove space) allSpaces
  where
    kingMove x y = uncurry max (distances x y) == 1

lineExtendingFrom :: Space -> (Int, Int) -> [Space]
lineExtendingFrom (file, rank) (fileIncrement, rankIncrement) =
  enumFromByIncrement file fileIncrement
    `zip` enumFromByIncrement rank rankIncrement

enumFromByIncrement :: (Enum a, Bounded a) => a -> Int -> [a]
enumFromByIncrement a inc = map toEnum [init, init + inc .. fromEnum $ maxBound `asTypeOf` a]
  where
    init = fromEnum a

allSpaces :: [Space]
allSpaces = (,) <$> files <*> ranks

ranks :: [Rank]
ranks = [One, Two, Three, Four, Five, Six, Seven, Eight]

files :: [File]
files = [A, B, C, D, E, F, G, H]

piecePositions :: Player -> Board -> [(PieceType, Space)]
piecePositions player board = do
  space <- allSpaces
  (player', pieceType) <- maybeToList $ Map.lookup space board
  guard $ player' == player
  return (pieceType, space)

other :: Player -> Player
other White = Black
other Black = White

pieceAt :: Board -> Space -> Maybe Piece
pieceAt board space = Map.lookup space board

enemyPiece :: Player -> Piece -> Maybe PieceType
enemyPiece ofPlayer (player, pieceType)
  | player == other ofPlayer = Just pieceType
  | otherwise = Nothing

applyMove :: Board -> Move -> Board
applyMove board move = Map.insert to (board ! from) (Map.delete from board)
  where
    to = toSpace move
    from = fromSpace move

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = fmap getFirst . foldMap (fmap First . f)
