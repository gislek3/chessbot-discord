{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Chess.Board (module Chess.Board) where


--Local imports
import Chess.Piece

--Other imports
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing, isJust, catMaybes, listToMaybe)
import qualified Data.Text as T
import qualified Data.Set as S
import Chess.Piece (getCircle)


-- Using Data.Map to inherit a lot of instances
type Board = M.Map Square Piece
type Square = (Int, Int)
type PositionedPiece = (Piece, Square)

{- instance Show Board where
  show = showB -}


--A move consists of a Piece making a move and the square it is moving to
data Move = Move {piece :: Piece, old_square :: Square, new_square :: Square}
    deriving (Show, Eq)

instance Ord Move where
    compare (Move _ o1 n1) (Move _ o2 n2) = compare (o1, n1) (o2, n2)



-- Initialize an empty chess board
empty :: Board
empty = M.empty

-- Place a piece on the board
place :: Square -> Piece -> Board -> Board
place s p b= if (isValidSquare s) then M.insert s p b else b

-- Clear a square on the board, does nothing if square is empty by default
clear :: Square -> Board -> Board
clear = M.delete

squareList :: [Square]
squareList = [(x,y) | x<-[0..7], y<-[0..7]]

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


justPiece :: SquareContent -> Piece
justPiece s = case s of
  Occupied p -> p
  _ -> error "SquareContent supplied does not contain a piece."

--Checks whether the contents of a square is a piece, comparable to isJust
isPiece :: SquareContent -> Bool
isPiece s = case s of
  Occupied _ -> True
  _ -> False

--Checks whether the contents of a square is null, comparable to isNothing
isEmpty :: SquareContent -> Bool
isEmpty s = case s of
  Empty -> True
  _ -> False

--isEmpty primed for square input
isEmptySquare :: Square -> Board -> Bool
isEmptySquare s b = isEmpty $ lookupB s b

isFriendlyTo :: Color -> SquareContent -> Bool
isFriendlyTo c s = isPiece s && (c==(pieceColor $ justPiece s))

isEnemyTo :: Color -> SquareContent -> Bool
isEnemyTo c s = not $ isFriendlyTo c s

isValidSquare :: Square -> Bool
isValidSquare (x,y) = (x >= 0 &&  x <= 7) && (y >= 0 && y <= 7)

isValidSquare' :: Move -> Bool
isValidSquare' (Move _ start end) = isValidSquare start && isValidSquare end

getAllPieces :: Board -> [Piece]
getAllPieces b = [justPiece (lookupB s b) | s <- squareList, isPiece (lookupB s b)]

-- Makes the board intoa human-readable Text representation
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

makeMove'' :: Square -> Square -> Board -> Maybe Board
makeMove'' start stop b = let p = lookupB start b in
  if isPiece p then makeMove (Move (justPiece p) start stop)  b else Nothing

-- Helper function to apply a move to the board and check legality
isLegalMove :: Move -> Board -> Bool
isLegalMove m b = isJust (makeMove m b)

--Return all valid moves for a given board
getAllMoves :: Board -> S.Set Move
getAllMoves b = S.union (getAllColorMoves White b) (getAllColorMoves Black b)

--Return all legal moves for a given board
getAllLegalMoves :: Board -> S.Set Move
getAllLegalMoves b = S.union (getAllLegalColorMoves White b) (getAllLegalColorMoves Black b)

--Return all valid moves for a specific color
getAllColorMoves :: Color -> Board -> S.Set Move
getAllColorMoves c b = S.unions $ [getPieceMoves (x, y) | x <- [0 .. 7], y <- [0 .. 7]]
  where
    getPieceMoves sq = case lookupB sq b of
      Occupied p@(Piece _ colorOfPiece _)| colorOfPiece == c -> getMoves (p,sq) b
      _ -> S.empty

--Return all legal moves for a given color
getAllLegalColorMoves :: Color -> Board -> S.Set Move
getAllLegalColorMoves c b = S.filter (`isLegalMove` b) (getAllColorMoves c b)

--This is probably a helper function
getNextSquare :: Delta -> Square -> Maybe Square
getNextSquare (rowD, colD) (row, col) = if isValidSquare (row+rowD, col+colD) then Just (row+rowD, col+colD)  else Nothing

getSurroundings :: Square -> [Square]
getSurroundings s = catMaybes [getNextSquare d s | d <- getCircle]

-- Function to find the king's square for a given color
getKingSquare :: Color -> Board -> Square
getKingSquare color = fst . head . filter isKing . M.assocs
  where
    isKing (_, piece) = pieceType piece == King && pieceColor piece == color


--Checks whether or not the king of a given color is in check by seeing if it's targeted by any of its enemies
kingIsInCheck :: Color -> S.Set Move -> Board -> Bool
kingIsInCheck friendlyColor enemyMoves b = not $ null [lookupB (new_square m) b | m <- S.toList enemyMoves,
            case lookupB (new_square m) b of
                Occupied (Piece King someColor _) -> someColor==friendlyColor
                _ -> False
            ]

kingIsInCheckmate :: Color -> Board -> Bool
kingIsInCheckmate friendlyColor b = let enemyMoves = getAllColorMoves (oppositeColor friendlyColor) b in
  (kingIsInCheck friendlyColor enemyMoves b) && not (canGetOutOfCheck friendlyColor b)


gameIsOver :: Board -> Bool
gameIsOver b = (kingIsInCheckmate White b) || (kingIsInCheckmate Black b)

--Given a moveset and a target square, return a subset containing the moves who end up at the target
movesThatReachSquare :: S.Set Move -> Square -> S.Set Move
movesThatReachSquare moves target = S.fromList [m | m <- S.toList moves, new_square m == target]

--Checks that after a given move; if the supplied color's king is in check
inCheckAfterMove :: Move -> Color -> Board -> Bool
inCheckAfterMove m myColor b =
    case makeMove m b of
        Just movedBoard -> kingIsInCheck myColor (getAllColorMoves (oppositeColor myColor) movedBoard) movedBoard
        Nothing -> False

canGetOutOfCheck :: Color -> Board -> Bool
canGetOutOfCheck c b = or [not $ inCheckAfterMove m c b | m <- S.toList (getAllColorMoves c b)]

hasLegalMoves :: Color -> S.Set Move -> Board -> Bool
hasLegalMoves c friend b = or [not (inCheckAfterMove m c b) | m <- S.toList friend]



--TODO: optimize code
-- TODO: comment
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


--TODO: optimize code
--Returns potential castling squares
castleKing :: PieceType -> Color -> Board -> Maybe Board
castleKing side c b = if notElem side [King, Queen] then Nothing else do
    let enemyMoves = S.toList $ getAllColorMoves (oppositeColor c) b
    let kingPosition = if c==White then (4,0) else (4,7)
    let piecePositions = if side==King then if c==White then [(4,0),(7,0)] else [(4,7),(7,7)]
                  else if c==White then [(4,0),(0,0)] else [(4,7),(0,7)]
    let squares = if side==King then if c==White then [(5,0),(6,0)] else [(5,7),(6,7)]
                  else if c==White then [(2,0),(3,0)] else [(1,7),(2,7),(3,7)]

    let safeSide = not $ or [new_square m `elem` squares || new_square m==kingPosition | m <- (enemyMoves)]
    let emptySide = and [lookupB s b == Empty | s <- squares]
    let unmovedSide = all (\s -> case lookupB s b of
                                      Occupied p -> not (hasMoved p)
                                      _ -> True) piecePositions

    if not (emptySide && safeSide && unmovedSide) then Nothing
    else if side==King then kingsideCastle b c
    else queensideCastle b c
      --TODO: make into a single function
      where
        kingsideCastle :: Board -> Color -> Maybe Board
        kingsideCastle board color = let y = if color==White then 0 else 7 in do
          let intermediate = makeMove' (Move (Piece Rook color True) (7,y) (5,y)) True board
          case intermediate of
            Nothing -> Nothing
            Just a -> makeMove' (Move (Piece King color True) (4,y) (6,y)) True a
        queensideCastle :: Board -> Color -> Maybe Board
        queensideCastle board color = let y = if color==White then 0 else 7 in do
          let intermediate = makeMove' (Move (Piece Rook color True) (0,y) (3,y)) True board
          case intermediate of
            Nothing -> Nothing
            Just a -> makeMove' (Move (Piece King color True) (4,y) (2,y)) True a



--Collect the possible moves for a piece in a given position
getMoves :: PositionedPiece -> Board -> S.Set Move
getMoves (p@(Piece pt c _), position) board =
    if pt==Pawn then getPawnMoves (p, position) board else
      let movementPattern = getMovementPattern pt
          deltasList = deltas movementPattern
          isContinuous = continous movementPattern
      in S.unions [followDelta p board position delta c isContinuous | delta <- deltasList]
