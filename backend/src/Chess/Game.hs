{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Chess.Game (play, GetAction, Board, Result (..), Player (..), Square, Rank (..), File (..), legalMoves, PieceType (..), ranks, files, startingBoard, fromMoves, Action (..), startingGame, attackingMoves, isUnderCheck, startingBoardStatus, simpleMoves) where

import Chess.Data
import Chess.Lib (singletonMaybe)
import Control.Monad (guard)
import Data.List (singleton)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, maybeToList)
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
playMove = undefined

-- playMove getAction game = g <$> getAction game
--  where
--    g (MoveAction move) = f $ applyMove game move
--    g Resign = (statusResigned, game) where statusResigned = Finished . Winner . other $ playerToMove game
--    f newGame = (postMoveStatus newGame, newGame)

matches :: NotatedMove -> Move -> Game -> Bool
matches (NotatedMove p1 (maybeOriginFile, maybeOriginRank) x1 d1 checkStatus) (Move (Movement (p2, (of2, or2)) (x2, d2))) game =
  p1 == p2
    && ( case x1 of
           Takes -> isJust x2
           To -> isNothing x2
       )
    && d1 == d2
    && all (== of2) maybeOriginFile
    && all (== or2) maybeOriginRank
    && checkBoardStatus' == checkStatus
  where
    checkBoardStatus' = checkType =<< checkBoardStatus (playerToMove game) (boardStatus game)

-- disambiguate :: Player -> BoardStatus -> NotatedMove -> [(Move, BoardStatus)]
-- disambiguate player boardStatus move = filter (matches move) $ legalMoves player boardStatus
--  where

checkBoardStatus :: Player -> BoardStatus -> Maybe CheckStatus
checkBoardStatus player boardStatus = case (legalMoves player boardStatus, isUnderCheck player (gameBoard boardStatus)) of
  ([], False) -> Just Stalemate
  ([], True) -> Just (CheckType Mate)
  (_, True) -> Just (CheckType Check)
  (_, _) -> Nothing

type GetAction f = Game -> f Action

startingGame :: Game
startingGame = Game {playerToMove = White, boardStatus = startingBoardStatus, fullMoveNumber = 1} where

startingBoardStatus :: BoardStatus
startingBoardStatus = BoardStatus {gameBoard = startingBoard, castlesAvailable = Set.fromList allCastles, enPassantSquare = Nothing, halfMoveClock = 0}

fromMoves :: [NotatedMove] -> Game -> Maybe Game
fromMoves = foldr f pure
  where
    f notatedMove g game = g =<< singletonMaybe [newGame | (move, newGame) <- legalMoves' game, matches notatedMove move newGame]
      where
        legalMoves' (Game {playerToMove, boardStatus}) = map advance' $ legalMoves playerToMove boardStatus
        advance' (move, boardStatus) = (move, advanceGame game boardStatus)

type PlayMove f = Game -> f (Status, Game)

finishedStatus :: Game -> Maybe EndStatus
finishedStatus = undefined

-- finishedStatus game = if null $ legalMoves game then Just (if kingIsUnderAttack then Checkmate else Stalemate) else Nothing
--  where
--    kingIsUnderAttack = King `elem` piecesUnderAttack
--    piecesUnderAttack = [piece | (_, (piece, _)) <- attackingMoves (playerToMove game) (gameBoard game)]

-- result :: Player -> EndStatus -> Result
-- result player Checkmate = Winner player
-- result _ Stalemate = Draw

postMoveStatus :: Game -> Status
postMoveStatus = undefined

-- postMoveStatus game = maybe Playing (Finished . result (playerToMove game)) $ finishedStatus game

updateBoardStatus :: Player -> Move -> BoardStatus -> BoardStatus
updateBoardStatus player move (BoardStatus {gameBoard, castlesAvailable, halfMoveClock}) = BoardStatus {gameBoard = newGameBoard, enPassantSquare, castlesAvailable = removeCastles castlesAvailable, halfMoveClock}
  where
    (enPassantSquare, removeCastles, halfMoveClock) = case move of
      Move movement -> (enPassantSquare', removeCastles, halfMoveClock)
        where
          enPassantSquare' = case (pieceType, fromRank, toRank) of
            (Pawn, Two, Four) -> Just (fromFile, Three)
            (Pawn, Seven, Five) -> Just (fromFile, Six)
            _ -> Nothing
          Movement from to = movement
          (_, (_, toRank)) = to
          (pieceType, (fromFile, fromRank)) = from
          removeCastles = foldr (.) id [Set.delete castle | (piece, square) <- from : forTakes to, castle <- invalidatedCastles piece square]
          invalidatedCastles Rook (A, One) = [(White, Queenside)]
          invalidatedCastles Rook (H, One) = [(White, Kingside)]
          invalidatedCastles Rook (A, Eight) = [(Black, Queenside)]
          invalidatedCastles Rook (H, Eight) = [(Black, Kingside)]
          invalidatedCastles King (E, One) = (White,) <$> castleSides
          invalidatedCastles King (E, Eight) = (Black,) <$> castleSides
          invalidatedCastles _ _ = []
          forTakes (Just a, b) = [(a, b)]
          forTakes (Nothing, _) = []
          halfMoveClock = if halfMoveClockResets then 0 else halfMoveClock + 1
          halfMoveClockResets = isCapture move || movePiece move == Pawn
      Castle _ -> (Nothing, remove Queenside . remove Kingside, halfMoveClock + 1)
        where
          remove side = Set.delete (player, side)
    newGameBoard = applyMoveToBoard player move gameBoard

applyMoveToBoard :: Player -> Move -> Board -> Board
applyMoveToBoard player (Move (Movement (movePiece, fromSquare) (_, toSquare))) = Map.insert toSquare (player, movePiece) . Map.delete fromSquare

-- legalMoves :: Game -> [(Move, Game)]
-- legalMoves game = [(move, advanceGame game newStatus) | ]

legalMoves :: Player -> BoardStatus -> [(Move, BoardStatus)]
legalMoves playerToMove boardStatus = [(move, newBoardStatus) | move <- attackingMoves' ++ simpleMoves', let newBoardStatus = updateBoardStatus playerToMove move boardStatus, legal playerToMove (gameBoard newBoardStatus)]
  where
    attackingMoves' = map (Move . fromAttackingMove) $ attackingMoves playerToMove board
    fromAttackingMove (Movement from (piece, to)) = Movement from (Just piece, to)
    board = gameBoard boardStatus
    simpleMoves' = map (Move . fromSimpleMove) $ simpleMoves playerToMove board
    fromSimpleMove (Movement from to) = Movement from (Nothing, to)

simpleMoves :: Player -> Board -> [SimpleMove]
simpleMoves playerToMove gameBoard = do
  (fromSquare, movePiece) <- pieceLocations playerToMove gameBoard
  lineOfMovement <- linesOfMovement (playerToMove, movePiece) fromSquare
  toSquare <- takeWhile unoccupied lineOfMovement
  return $ Movement (movePiece, fromSquare) toSquare
  where
    unoccupied space = isNothing $ Map.lookup space gameBoard

legal :: Player -> Board -> Bool
legal playerToMove gameBoard = not $ isUnderCheck playerToMove gameBoard

attackingMoves :: Player -> Board -> [AttackingMove]
attackingMoves player board = do
  (fromSquare, movePiece) <- pieceLocations player board
  spaces <- linesOfAttack (player, movePiece) fromSquare
  maybeToList $ do
    (toSquare, (player', pieceType)) <- firstJust (fmapSnd . pieceAt $ board) spaces
    guard $ player' == other player
    return $ Movement (movePiece, fromSquare) (pieceType, toSquare)
  where
    fmapSnd f a = (,) a <$> f a
    pieceAt board space = Map.lookup space board

isUnderCheck :: Player -> Board -> Bool
isUnderCheck player board = King `elem` piecesUnderAttack
  where
    piecesUnderAttack = [piece | piece <- map pieceOf $ attackingMoves (other player) board]
      where
        pieceOf (Movement {to}) = fst to

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

advanceGame :: Game -> BoardStatus -> Game
advanceGame (Game {playerToMove, fullMoveNumber}) newBoardStatus = Game {playerToMove = other playerToMove, boardStatus = newBoardStatus, fullMoveNumber = if playerToMove == Black then fullMoveNumber + 1 else fullMoveNumber}

isCapture :: Move -> Bool
isCapture = isJust . pieceUnderAttack

pieceUnderAttack :: Move -> Maybe PieceType
pieceUnderAttack (Move (Movement {to})) = fst to

movePiece :: Move -> PieceType
movePiece (Move (Movement {from})) = fst from

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = fmap getFirst . foldMap (fmap First . f)
