{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Chess.Game (play, GetAction, Board, Result (..), Player (..), Square, Rank (..), File (..), legalMoves, PieceType (..), ranks, files, startingBoard, fromMoves, Action (..), startingGame, attackingMoves, isUnderCheck, simpleMoves, applyMoveToBoard) where

import Chess.Data
import Chess.Lib (singletonMaybe)
import Control.Monad (guard)
import Data.List (singleton)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, maybeToList)
import Data.Semigroup (First (..))
import qualified Data.Set as Set
import Debug.Trace (trace)

data Result
  = Winner Player
  | Draw
  deriving (Eq, Show)

data Status
  = Playing
  | Finished Result

data Action = MoveAction Move | Resign

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
  fromPieces $
    [ ((file, rank), (player, pieceType))
      | (rank, player, row) <-
          [ (One, White, otherPieces),
            (Two, White, pawns),
            (Seven, Black, pawns),
            (Eight, Black, otherPieces)
          ],
        (file, pieceType) <- files `zip` row
    ]
  where
    pawns = replicate 8 Pawn
    otherPieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

playMove :: (Functor f) => GetAction f -> PlayMove f
playMove = undefined

matches :: NotatedMove -> Move -> Maybe CheckType -> Bool
matches (NotatedMove p1 (maybeOriginFile, maybeOriginRank) x1 d1 checkStatus) (Move (p2, (of2, or2)) (x2, d2)) maybeCheckType =
  p1 == p2
    && ( case x1 of
           Takes -> isJust x2
           To -> isNothing x2
       )
    && d1 == d2
    && all (== of2) maybeOriginFile
    && all (== or2) maybeOriginRank
    && maybeCheckType == checkStatus

checkBoardStatus :: Game -> Maybe CheckStatus
checkBoardStatus game = case (null $ legalMoves game, isUnderCheck player board) of
  (True, False) -> Just Stalemate
  (True, True) -> Just (CheckType Mate)
  (False, True) -> Just (CheckType Check)
  (False, False) -> Nothing
  where
    player = playerToMove game
    board = gameBoard game

type GetAction f = Game -> f Action

startingGame :: Game
startingGame = Game {playerToMove = White, gameBoard = startingBoard, castlesAvailable = Set.fromList allCastles, enPassantSquare = Nothing, halfMoveClock = 0, fullMoveNumber = 1} where

fromMoves :: [NotatedMove] -> Game -> Maybe Game
fromMoves = foldr f pure
  where
    f notatedMove g game = g =<< singletonMaybe [newGame | (move, newGame) <- legalMoves game, matches notatedMove move (maybeCheckType newGame)]
    maybeCheckType game = checkType =<< checkBoardStatus game

type PlayMove f = Game -> f (Status, Game)

applyMoveToBoard :: Player -> Move -> Board -> Board
applyMoveToBoard player (Move (movePiece, fromSquare) (_, toSquare)) = Map.insert toSquare (player, movePiece) . Map.delete fromSquare

legalMoves :: Game -> [(Move, Game)]
legalMoves (Game {gameBoard = board, playerToMove = player, castlesAvailable, halfMoveClock, fullMoveNumber}) = withInput advanceGame =<< attackingMoves' ++ simpleMoves'
  where
    attackingMoves' = map fromAttackingMove $ attackingMoves player board
    simpleMoves' = map fromSimpleMove $ simpleMoves player board
    withInput f a = (a,) <$> f a
    advanceGame move = newGame <$ guard legal
      where
        legal = not $ isUnderCheck player newBoard
        newBoard = applyMoveToBoard player move board
        newGame = Game {playerToMove = other player, gameBoard = newBoard, enPassantSquare, castlesAvailable = removeCastles castlesAvailable, halfMoveClock = updateHalfMoveClock halfMoveClock, fullMoveNumber = increment fullMoveNumber}
        increment = case player of
          Black -> (+ 1)
          White -> id
        removeCastles = foldr (.) id [Set.delete castle | castle <- invalidatedCastles player move]
        updateHalfMoveClock = case move of
          Move (Pawn, _) _ -> const 0
          Move _ (Just _, _) -> const 0
          _ -> (+ 1)
        enPassantSquare = case move of
          Move (Pawn, (fromFile, Two)) (_, (_, Four)) -> Just (fromFile, Three)
          Move (Pawn, (fromFile, Seven)) (_, (_, Five)) -> Just (fromFile, Six)
          _ -> Nothing

invalidatedCastles :: Player -> Move -> [CastleLocation]
invalidatedCastles player (Castle _) = (player,) <$> castleSides
invalidatedCastles _ (Move (piece, from) (taking, to)) = castlesFrom piece from <> castlesTo taking to
  where
    castlesFrom Rook (A, One) = [(White, Queenside)]
    castlesFrom Rook (H, One) = [(White, Kingside)]
    castlesFrom Rook (A, Eight) = [(Black, Queenside)]
    castlesFrom Rook (H, Eight) = [(Black, Kingside)]
    castlesFrom King (E, One) = (White,) <$> castleSides
    castlesFrom King (E, Eight) = (Black,) <$> castleSides
    castlesFrom _ _ = []
    castlesTo (Just piece) = castlesFrom piece
    castlesTo Nothing = const []

simpleMoves :: Player -> Board -> [SimpleMove]
simpleMoves playerToMove gameBoard = do
  (fromSquare, movePiece) <- pieceLocations playerToMove gameBoard
  lineOfMovement <- linesOfMovement (playerToMove, movePiece) fromSquare
  toSquare <- takeWhile unoccupied lineOfMovement
  return ((movePiece, fromSquare), toSquare)
  where
    unoccupied space = isNothing $ Map.lookup space gameBoard

attackingMoves :: Player -> Board -> [AttackingMove]
attackingMoves player board = do
  (fromSquare, movePiece) <- pieceLocations player board
  spaces <- linesOfAttack (player, movePiece) fromSquare
  maybeToList $ do
    (toSquare, (player', pieceType)) <- firstJust (fmapSnd pieceAt) spaces
    guard $ player' == other player
    return ((movePiece, fromSquare), (pieceType, toSquare))
  where
    fmapSnd f a = (,) a <$> f a
    pieceAt space = Map.lookup space board

isUnderCheck :: Player -> Board -> Bool
isUnderCheck player board = King `elem` piecesUnderAttack
  where
    piecesUnderAttack = [piece | piece <- map pieceOf $ attackingMoves (other player) board]
      where
        pieceOf (_, (piece, _)) = piece

linesOfMovement :: Piece -> Square -> [[Square]]
linesOfMovement (player, pieceType) = case pieceType of
  Pawn -> singleton . pawnMoves player
  Knight -> map singleton . knightMoves
  Bishop -> bishopLines
  Rook -> rookLines
  King -> map singleton . kingMoves
  Queen -> queenLines

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

other :: Player -> Player
other White = Black
other Black = White

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = fmap getFirst . foldMap (fmap First . f)
