{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Chess.Board (module Chess.Board) where


--Local imports
import Chess.Piece
    ( Delta,
      Piece(Piece, pieceColor, pieceType, hasMoved),
      PieceType(Pawn, Rook, Knight, Bishop, Queen, King),
      Color(..),
      startP,
      pieceToChar,
      getMovementPattern,
      MovementPattern(deltas, continous),
      oppositeColor)

--Other imports
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing, isJust)
import qualified Data.Text as T
import qualified Data.Set as S


-- Define the ChessBoard as a map from Square to Maybe Piece
type Board = M.Map Square Piece

type Square = (Int, Int)

--A move consists of a Piece making a move and the square it is moving to
data Move = Move {piece :: Piece, old_square :: Square, new_square :: Square}
    deriving (Show, Eq)

instance Ord Move where
    compare (Move _ o1 n1) (Move _ o2 n2) = compare (o1, n1) (o2, n2)

{- instance Show Board where
  show = showB -}

-- Initialize an empty chess board
empty :: Board
empty = M.empty

-- Place a piece on the board
place :: Square -> Piece -> Board -> Board
place s p b= if (isValidSquare s) then M.insert s p b else b

-- Clear a square on the board, does nothing if square is empty by default
clear :: Square -> Board -> Board
clear = M.delete

--data SquareContent = Illegal | Empty | Occupied Piece | EnemyPiece Piece | FriendlyPiece Piece
--  deriving (Show, Eq)

data SquareContent = Illegal | Empty | Occupied Piece deriving (Show, Eq)


-- TODO: Initialize the initial board, in the starting position
startingBoard :: Board
startingBoard = foldr (\(s, p) board -> place s p board) empty startingPieces
  where
    -- List of pieces with their starting positions
    startingPieces :: [(Square, Piece)]
    startingPieces =
      -- Place white pieces at the bottom row (0) and the second row from the bottom (1)
      [ ((0, 0), startP Rook White), ((7, 0), startP Rook White),
        ((1, 0), startP Knight White), ((6, 0), startP Knight White),
        ((2, 0), startP Bishop White), ((5, 0), startP Bishop White),
        ((3, 0), startP Queen White), ((4, 0), startP King White)
      ]
      ++ [((x, 1), startP Pawn White) | x <- [0..7]] -- Second row from the bottom

      -- Place black pieces at the top row (7) and the second row from the top (6)
      ++ [ ((0, 7), startP Rook Black), ((7, 7), startP Rook Black),
           ((1, 7), startP Knight Black), ((6, 7), startP Knight Black),
           ((2, 7), startP Bishop Black), ((5, 7), startP Bishop Black),
           ((3, 7), startP Queen Black), ((4, 7), startP King Black)
      ]
      ++ [((x, 6), startP Pawn Black) | x <- [0..7]] -- Second row from the top



--Returns the content of the square as a SquareContent
lookupB :: Square -> Board -> SquareContent
lookupB s b
  | not $ isValidSquare s = Illegal
  | isJust (M.lookup s b) = Occupied $ fromJust (M.lookup s b)
  | isNothing (M.lookup s b) = Empty
  | otherwise = error "Invalid state reached in lookupB"


isValidSquare :: Square -> Bool
isValidSquare (x,y) = (x >= 0 &&  x <= 7) && (y >= 0 && y <= 7)

isValidSquare' :: Move -> Bool
isValidSquare' (Move _ start end) = isValidSquare start && isValidSquare end

-- Converts the chess board to a human-readable string representation.
showB :: Board -> T.Text
showB b = "```\n" <> topMargin <> "\n" <> T.intercalate "\n" boardRows <> "\n" <> bottomMargin <> "\n```"
  where
    topMargin = "+-A--B--C--D--E--F--G--H-+"
    bottomMargin = topMargin

    -- Generate rows starting from the bottom (7 to 0)
    boardRows = [T.pack (show (y+1)) <> "|" <> rowToString y <> "|" <> T.pack (show (y+1)) | y <- [7,6..0]]

    rowToString :: Int -> T.Text
    rowToString y = T.concat ["|" <>
        T.singleton (
            case lookupB (x, y) b of
                Illegal -> error "Illegal square found when trying to show the board."
                Empty -> '＋'
                Occupied somePiece -> pieceToChar somePiece
        ) | x <- [0..7]]


evaluatePromotions :: Board -> Board
evaluatePromotions = M.mapWithKey promoteIfPossible
  where
    promoteIfPossible :: Square -> Piece -> Piece
    promoteIfPossible s p = case p of
      Piece Pawn White _ -> if s `elem` ([(x,7)|x<-[0..7]]) then Piece Queen White True else p
      Piece Pawn Black _ -> if s `elem` ([(x,0)|x<-[0..7]]) then Piece Queen Black True else p
      _ -> p


-- Applies a move to the board if the move is legal
makeMove' :: Move -> Bool -> Board -> Maybe Board
makeMove' m@(Move _ start stop ) forced board =
    if (isValidSquare start && isValidSquare stop) && is_legal start stop board then
        let
            boardWithoutOldPiece = clear (old_square m) board
            boardWithNewPiece = place (new_square m) ((piece m){hasMoved=True}) boardWithoutOldPiece
        in
            Just $ evaluatePromotions boardWithNewPiece
    else
        Nothing
  where
    is_legal :: Square -> Square -> Board -> Bool
    is_legal one two b = forced || case lookupB start b of
      Occupied p -> do
        let movingPieceMoves = getMoves (p, one) b
        let forceMoveOnBoard = makeMove' m True board
        isJust forceMoveOnBoard && S.member (Move p one two) movingPieceMoves -- && not (kingIsInCheck (pieceColor movingPiece) (fromJust forceMoveOnBoard)))
      Empty -> False
      Illegal -> error "Illegal squares passed the initial check."

makeMove :: Move -> Board -> Maybe Board
makeMove m = makeMove' m False


{- kingIsInCheck :: Color -> Board -> Bool
kingIsInCheck c b = not (null listOfAllTargets)
  where
    listOfAllTargets = [lookupB (new_square m) b | m <- S.toList (getAllColorMoves (oppositeColor c) b),
      case lookupB (new_square m) b of
        Occupied (Piece King someColor _) -> someColor==c
        _ -> False
      ] -}




--Return all legal moves for a given board
getAllMoves :: Board -> S.Set Move
getAllMoves b = S.union (getAllColorMoves White b) (getAllColorMoves Black b)

--Return all legal moves for a specific color
getAllColorMoves :: Color -> Board -> S.Set Move
getAllColorMoves c b = S.unions $ [getPieceMoves (x, y) | x <- [0 .. 7], y <- [0 .. 7]]
  where
    getPieceMoves sq = case lookupB sq b of
      Occupied p@(Piece {pieceColor=colorOfPiece})| colorOfPiece == c -> getMoves (p,sq) b
      _ -> S.empty


getNextSquare :: Delta -> Square -> Maybe Square
getNextSquare (rowD, colD) (row, col) = if isValidSquare (row+rowD, col+colD) then Just (row+rowD, col+colD)  else Nothing


-- Update the followDelta function to consider continuous movement
followDelta :: Piece -> Board -> Square -> Delta -> Color -> Bool -> S.Set Move
followDelta p board start delta c continuous = go start
  where
    go sq
      | not continuous = case getNextSquare delta sq of
          Just nextSq -> case lookupB nextSq board of
            Illegal -> S.empty
            Empty -> S.insert (Move p start nextSq) S.empty
            Occupied (Piece _ colorAtSquare _) ->
              if colorAtSquare /= c then S.singleton (Move p start nextSq) else S.empty
          Nothing -> S.empty
      | otherwise = case getNextSquare delta sq of
          Nothing -> S.empty
          Just nextSq -> case lookupB nextSq board of
            Illegal -> S.empty
            Empty -> S.insert (Move p start nextSq) (go nextSq)
            Occupied (Piece _ colorAtSquare _) ->
              if colorAtSquare /= c then S.singleton (Move p start nextSq) else S.empty

type PositionedPiece = (Piece, Square)

-- Adjust getMoves to extract deltas from MovementPattern and handle continuous movement
getPawnMoves :: PositionedPiece -> Board -> S.Set Move
getPawnMoves ((p@(Piece {pieceType = Pawn, pieceColor = c, hasMoved = hm}), (x, y))) board =
  let
    -- Calculate forward moves depending on color and whether the pawn has moved before
    forwardDeltas = if c == White then [(0, 1)] ++ if not hm then [(0, 2)] else [] else [(0, -1)] ++ if not hm then [(0, -2)] else []
    forwardMoves = [(Move p (x, y) (x + dx, y + dy)) | (dx, dy) <- forwardDeltas, lookupB (x + dx, y + dy) board == Empty]

    -- Calculate attack moves
    attackDeltas = if c == White then [(1, 1), (-1, 1)] else [(1, -1), (-1, -1)]
    attackMoves = [(Move p (x, y) (x + dx, y + dy)) | (dx, dy) <- attackDeltas, case lookupB (x + dx, y + dy) board of
                      Occupied pAtSquare -> pieceColor pAtSquare == oppositeColor c
                      _ -> False]

  in S.fromList (forwardMoves ++ attackMoves)
getPawnMoves _ _= error "getPawnMoves called with non-pawn argument"


--Returns potential castling squares
castleKing :: PieceType -> Color -> Board -> Maybe Board
castleKing side c b = if notElem side [King, Queen] then Nothing else do
    let kingSideSquares = if c==White then [(5,0),(6,0)] else [(5,7),(6,7)]
    let queenSideSquares = if c==White then [(2,0),(3,0)] else [(2,7),(3,7)]
    let enemyMoves = S.toList $ getAllColorMoves (oppositeColor c) b
    let safeKingSide = not $ or [(new_square m) `elem` kingSideSquares | m <- enemyMoves]
    let emptyKingSide = and [(lookupB s b) == Empty | s <- kingSideSquares]
    let kingSideUnmoved = all (\s -> case lookupB s b of
                                      Occupied p -> not (hasMoved p)
                                      _ -> True) kingSideSquares   
    let kingOK = emptyKingSide&&safeKingSide&&kingSideUnmoved
    let emptyQueenSide = and [(lookupB s b) == Empty | s <- queenSideSquares]
    let safeQueenSide = and [(new_square m) `elem` queenSideSquares | m <- enemyMoves]
    let queenSideUnmoved = all (\s -> case lookupB s b of
                                      Occupied p -> not (hasMoved p)
                                      _ -> True) queenSideSquares
    let queenOK = emptyQueenSide&&safeQueenSide&&queenSideUnmoved

    if side==King&&kingOK then kingsideCastle b c
    else if side==Queen&&queenOK then queensideCastle b c
    else Nothing
      where
        kingsideCastle :: Board -> Color -> Maybe Board
        kingsideCastle board color = let y = if color==White then 0 else 7 in do
          intermediate <- makeMove' (Move (Piece Rook color True) (7,y) (5,y)) True board
          makeMove' (Move (Piece King color True) (4,y) (6,y)) True intermediate
        queensideCastle :: Board -> Color -> Maybe Board
        queensideCastle board color = let y = if color==White then 0 else 7 in do
          intermediate <- makeMove' (Move (Piece Rook color True) (0,y) (3,y)) True board
          makeMove' (Move (Piece King color True) (4,y) (2,y)) True intermediate
        





--Collect the possible moves for a piece in a given position
getMoves :: PositionedPiece -> Board -> S.Set Move
getMoves (p@(Piece pt c _), position) board =
    if pt==Pawn then getPawnMoves (p, position) board else
      let movementPattern = getMovementPattern pt
          deltasList = deltas movementPattern
          isContinuous = continous movementPattern
      in S.unions [followDelta p board position delta c isContinuous | delta <- deltasList]

