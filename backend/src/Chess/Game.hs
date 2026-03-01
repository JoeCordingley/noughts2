{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Chess.Game (play, GetAction, Board, Result (..), Player (..), Square, Rank (..), File (..), legalMoves, PieceType (..), ranks, files, startingBoard, Action (..), startingGame, attackingMoves, isUnderCheck, nonAttackingMoves, applyMoveToBoard, gameCheckType, firstJust) where

import Chess.Data
import Chess.Lib (firstJust, withInputF)
import Control.Monad (guard, (<=<))
import Data.List (singleton)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set

data Result
  = Winner Player
  | Draw
  deriving (Eq, Show)

data Status
  = Playing
  | Finished Result

data Action = MoveAction (ChessMove CommonMove) | Resign

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
  fromJust . fromPieces $
    [ ((file', rank'), (player, pieceType'))
      | (rank', player, row) <-
          [ (One, White, otherPieces),
            (Two, White, pawns),
            (Seven, Black, pawns),
            (Eight, Black, otherPieces)
          ],
        (file', pieceType') <- files `zip` row
    ]
  where
    pawns = replicate 8 Pawn
    otherPieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

playMove :: GetAction f -> PlayMove f
playMove = undefined

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

gameCheckType :: Game -> Maybe CheckType
gameCheckType = checkType <=< checkBoardStatus

type PlayMove f = Game -> f (Status, Game)

applyMoveToBoard :: Player -> ChessMove MoveAndPromotion -> Maybe Square -> Board -> Board
applyMoveToBoard player (RegularMove (MoveAndPromotion (CommonMove Pawn fromSquare toSquare _) _)) (Just enPassantSquare) board
  | toSquare == enPassantSquare =
      board {pieceMap = Map.delete fromSquare . Map.delete passedPawnSquare . Map.insert toSquare (player, Pawn) $ pieceMap board}
  where
    passedPawnSquare = case player of
      White -> (file', Five)
      Black -> (file', Four)
    (file', _) = toSquare
applyMoveToBoard player (RegularMove (MoveAndPromotion (CommonMove piece' fromSquare toSquare _) promotion)) _ board = applyMovementToBoard (Movement ((player, fromMaybe piece' promotion), fromSquare) toSquare) board
applyMoveToBoard player (Castle side) _ board = case side of
  Queenside -> applyMovementToBoard (Movement ((player, King), (E, homeRow)) (C, homeRow)) . applyMovementToBoard (Movement ((player, Rook), (A, homeRow)) (D, homeRow)) $ board
  Kingside -> applyMovementToBoard (Movement ((player, King), (E, homeRow)) (G, homeRow)) . applyMovementToBoard (Movement ((player, Rook), (H, homeRow)) (F, homeRow)) $ board
  where
    homeRow = case player of
      White -> One
      Black -> Eight

applyMovementToBoard :: Movement (Piece, Square) Square -> Board -> Board
applyMovementToBoard (Movement (p@(_, _), from) to) b@Board {pieceMap, whiteKing, blackKing} =
  b
    { pieceMap = Map.delete from . Map.insert to p $ pieceMap,
      whiteKing = if p == (White, King) then to else whiteKing,
      blackKing = if p == (Black, King) then to else blackKing
    }

legalMoves :: Game -> [(ChessMoveInternal, Game)]
legalMoves (Game {gameBoard = board, playerToMove = player, castlesAvailable, halfMoveClock, fullMoveNumber, enPassantSquare}) = withInputF advanceGame =<< regularMoves ++ castles'
  where
    regularMoves = [RegularMove (MoveAndPromotion {move, promotion}) | move <- attackingMoves' ++ simpleMoves' ++ enPassantMoves', promotion <- promotions (movingPiece move) (rank . toSquare $ move)]
    promotions Pawn rank' | rank' == promotionRank = map Just majorPieces
    promotions _ _ = [Nothing]
    promotionRank = case player of
      White -> Eight
      Black -> One
    attackingMoves' = map fromAttackingMove $ attackingMoves player board
    simpleMoves' = map fromNonAttackingMove $ nonAttackingMoves player board
    enPassantMoves' = map fromEnPassantMove $ maybe [] (enPassantMoves player board) enPassantSquare
    castles' = map Castle $ castles player board castlesAvailable
    advanceGame :: ChessMove MoveAndPromotion -> [Game]
    advanceGame move = newGame <$ guard legal
      where
        legal = not $ isUnderCheck player newBoard
        newBoard = applyMoveToBoard player move enPassantSquare board
        newGame = Game {playerToMove = other player, gameBoard = newBoard, enPassantSquare = enPassantSquare', castlesAvailable = removeCastles castlesAvailable, halfMoveClock = updateHalfMoveClock halfMoveClock, fullMoveNumber = increment fullMoveNumber}
        increment = case player of
          Black -> (+ 1)
          White -> id
        removeCastles = foldr (.) id [Set.delete castle | castle <- invalidatedCastles player move]
        updateHalfMoveClock = case move of
          RegularMove (MoveAndPromotion {move = CommonMove {movingPiece = Pawn}}) -> const 0
          RegularMove (MoveAndPromotion {move = CommonMove {capturedPiece = Just _}}) -> const 0
          _ -> (+ 1)
        enPassantSquare' = case move of
          RegularMove (MoveAndPromotion {move = CommonMove {movingPiece = Pawn, fromSquare = (file', fromRank), toSquare = (_, toRank)}}) -> case (fromRank, toRank) of
            (Two, Four) -> Just (file', Three)
            (Seven, Five) -> Just (file', Six)
            _ -> Nothing
          _ -> Nothing

castles :: Player -> Board -> Set CastleLocation -> [CastleSide]
castles player board castlesAvailable = [castle | castle <- castleSides, Set.member (player, castle) castlesAvailable, inCastlingPosition (player, castle) board]

inCastlingPosition :: CastleLocation -> Board -> Bool
inCastlingPosition (player, castle) board = all empty interveningSquares && all notAttacked kingSquares
  where
    interveningSquares =
      (,homeRow) <$> case castle of
        Queenside -> [B, C, D]
        Kingside -> [F, G]
    homeRow = case player of
      White -> One
      Black -> Eight
    empty square' = isNothing $ Map.lookup square' (pieceMap board)
    notAttacked sq = not $ isSquareAttackedBy (other player) sq board
    kingSquares =
      (,homeRow)
        <$> E : case castle of
          Queenside -> [D, C]
          Kingside -> [F, G]

invalidatedCastles :: Player -> ChessMoveInternal -> [CastleLocation]
invalidatedCastles player (Castle _) = (player,) <$> castleSides
invalidatedCastles _ (RegularMove (MoveAndPromotion (CommonMove movePiece from to taking) _)) = castlesFrom movePiece from <> castlesTo taking to
  where
    castlesFrom Rook (A, One) = [(White, Queenside)]
    castlesFrom Rook (H, One) = [(White, Kingside)]
    castlesFrom Rook (A, Eight) = [(Black, Queenside)]
    castlesFrom Rook (H, Eight) = [(Black, Kingside)]
    castlesFrom King (E, One) = (White,) <$> castleSides
    castlesFrom King (E, Eight) = (Black,) <$> castleSides
    castlesFrom _ _ = []
    castlesTo (Just piece') = castlesFrom piece'
    castlesTo Nothing = const []

nonAttackingMoves :: Player -> Board -> [NonAttackingMove]
nonAttackingMoves playerToMove gameBoard = do
  (fromSquare, movePiece) <- pieceLocations playerToMove gameBoard
  lineOfMovement <- linesOfMovement (playerToMove, movePiece) fromSquare
  toSquare <- takeWhile unoccupied lineOfMovement
  return $ nonAttackingMove movePiece fromSquare toSquare
  where
    unoccupied space = isNothing $ Map.lookup space (pieceMap gameBoard)

attackingMoves :: Player -> Board -> [AttackingMove]
attackingMoves player board = do
  (fromSquare, movePiece) <- pieceLocations player board
  spaces <- linesOfAttack (player, movePiece) fromSquare
  maybeToList $ do
    (toSquare, (player', pieceType')) <- firstJust (fmapSnd pieceAt) spaces
    guard $ player' == other player
    return $ attackingMove movePiece fromSquare pieceType' toSquare
  where
    fmapSnd f a = (a,) <$> f a
    pieceAt space = Map.lookup space (pieceMap board)

enPassantMoves :: Player -> Board -> Square -> [EnPassantMove]
enPassantMoves player board square' = do
  (fromSquare, movePiece) <- pieceLocations player board
  guard $ movePiece == Pawn
  toSquare <- pawnAttacks player fromSquare
  guard $ toSquare == square'
  return $ Movement fromSquare toSquare

isUnderCheck :: Player -> Board -> Bool
isUnderCheck player board = isSquareAttackedBy (other player) (findKing player board) board

findKing :: Player -> Board -> Square
findKing White = whiteKing
findKing Black = blackKing

isSquareAttackedBy :: Player -> Square -> Board -> Bool
isSquareAttackedBy attacker sq board =
  any checkKnight knightSquares
    || any checkPawn pawnAttackSquares
    || any checkKing kingSquares
    || any (checkSlider [Rook, Queen]) rookLines'
    || any (checkSlider [Bishop, Queen]) bishopLines'
  where
    checkPiece pt pSq = Map.lookup pSq (pieceMap board) == Just (attacker, pt)
    checkKnight = checkPiece Knight
    checkPawn = checkPiece Pawn
    checkKing = checkPiece King
    checkSlider pts line = case firstJust (\s -> (s,) <$> Map.lookup s (pieceMap board)) line of
      Just (_, (p, pt)) -> p == attacker && pt `elem` pts
      Nothing -> False
    knightSquares = concat $ linesOfMovement (attacker, Knight) sq
    kingSquares = concat $ linesOfMovement (attacker, King) sq
    pawnAttackSquares = case (attacker, rank sq) of
      (White, One) -> []
      (Black, Eight) -> []
      _ -> pawnAttacks (other attacker) sq
    rookLines' = linesOfMovement (attacker, Rook) sq
    bishopLines' = linesOfMovement (attacker, Bishop) sq

linesOfMovement :: Piece -> Square -> [[Square]]
linesOfMovement = curry (linesOfMovementMap !)
  where
    linesOfMovementMap = Map.fromList [((piece', square'), linesOfMovement' piece' square') | (piece', square') <- validPiecePlacements]
      where
        linesOfMovement' (player, pieceType') = case pieceType' of
          Pawn -> singleton . pawnMoves player
          Knight -> map singleton . knightMoves
          Bishop -> bishopLines
          Rook -> rookLines
          King -> map singleton . kingMoves
          Queen -> queenLines

linesOfAttack :: Piece -> Square -> [[Square]]
linesOfAttack = curry (linesOfAttackMap !)
  where
    linesOfAttackMap = Map.fromList [((piece', square'), linesOfAttack' piece' square') | (piece', square') <- validPiecePlacements]
      where
        linesOfAttack' (player, pieceType') = case pieceType' of
          Pawn -> map singleton . pawnAttacks player
          _ -> linesOfMovement (player, pieceType')

validPiecePlacements :: [(Piece, Square)]
validPiecePlacements = [(piece', square') | piece' <- pieces, square' <- validPositions (pieceType piece')]

validPositions :: PieceType -> [Square]
validPositions Pawn = pawnSquares
validPositions _ = allSquares

pawnMoves :: Player -> Square -> [Square]
pawnMoves = traverse . rankDestinations
  where
    rankDestinations White Two = [Three, Four]
    rankDestinations White rank' = [succ rank']
    rankDestinations Black Seven = [Six, Five]
    rankDestinations Black rank' = [pred rank']

pawnAttacks :: Player -> Square -> [Square]
pawnAttacks player = pawnAttacks'
  where
    pawnAttacks' (A, rank') = [(B, advanceOne rank')]
    pawnAttacks' (H, rank') = [(G, advanceOne rank')]
    pawnAttacks' (file', rank') = map (,advanceOne rank') [pred file', succ file']
    advanceOne rank' = case player of
      White -> succ rank'
      Black -> pred rank'

knightMoves :: Square -> [Square]
knightMoves space = filter (knightMove space) allSquares
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
    diagonalDirections = (,) <$> [LT, GT] <*> [LT, GT]

queenLines :: Square -> [[Square]]
queenLines space = bishopLines space ++ rookLines space

rookLines :: Square -> [[Square]]
rookLines space = map (lineExtendingFrom space) orthogonalDirections
  where
    orthogonalDirections = map (,EQ) [LT, GT] ++ map (EQ,) [LT, GT]

kingMoves :: Square -> [Square]
kingMoves space = filter (kingMove space) allSquares
  where
    kingMove x y = uncurry max (distances x y) == 1

lineExtendingFrom :: Square -> (Ordering, Ordering) -> [Square]
lineExtendingFrom (file', rank') (fileIncrement, rankIncrement) =
  filesFrom fileIncrement file'
    `zip` ranksFrom rankIncrement rank'

other :: Player -> Player
other White = Black
other Black = White
