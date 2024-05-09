{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Chess.Board (Board, Move(..), Square, SquareContent(..), showB, startingBoard, lookupB, isPiece, justPiece, isValidSquare,  castleKing, kingIsInCheck, canGetOutOfCheck, hasLegalMoves, makeMove, getAllMoves, getAllColorMoves, getSurroundings, getAllPieces, getKingSquare, getAllLegalColorMoves, getMoves) where


--Imports
import Chess.Piece
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing, isJust, catMaybes)
import qualified Data.Text as T
import qualified Data.Set as S


{-
The board is defined as Map from Square to Piece. Each move results in a new Map reflecting
the updated state,  avoiding side effects and fostering referential transparency. 
I get to inherit a lot of instances and efficiency in use of higher-order functions than
if I were to implement my own structure.
-}


-- | Defining the board as a type alias
type Board = M.Map Square Piece

-- | Simple alias for squares. We start bottom left, so a1 is (0,0) and h8 is (7,7)
type Square = (Int, Int)

-- | Composite for a Piece at a given position
type PositionedPiece = (Piece, Square)

-- | A square can eiher by occupied by a piece on a board, an empty space on a board, or an invalid/illegal square. Doing this instead of Maybe Square allows me to distinguish beween empty squares and squares outside teh board.
data SquareContent = Illegal | Empty | Occupied Piece deriving (Show, Eq)

-- | A move consists of a Piece making a move and the square it is moving to
data Move = Move {piece :: Piece, old_square :: Square, new_square :: Square} deriving (Show, Eq)

instance Ord Move where compare (Move _ o1 n1) (Move _ o2 n2) = compare (o1, n1) (o2, n2)



-- | Initialize an empty chess board
empty :: Board
empty = M.empty

-- | Place a piece on the board
place :: Square -> Piece -> Board -> Board
place s p b= if (isValidSquare s) then M.insert s p b else b

-- | Clear a square on the board, does nothing if square is empty by default
clear :: Square -> Board -> Board
clear = M.delete


-- | Initialize the initial board, in the starting position
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



-- | Returns the content of a board's square as a SquareContent
lookupB :: Square -> Board -> SquareContent
lookupB s b
  | not $ isValidSquare s = Illegal
  | isJust (M.lookup s b) = Occupied $ fromJust (M.lookup s b)
  | isNothing (M.lookup s b) = Empty
  | otherwise = error "Invalid state reached in lookupB"

-- | fromJust-inspired approach to retrieving a piece from a SquareContent
justPiece :: SquareContent -> Piece
justPiece s = case s of
  Occupied p -> p
  _ -> error "SquareContent supplied does not contain a piece."

-- | isJust-inspired approach, checks whether the contents of a square is a piece.
isPiece :: SquareContent -> Bool
isPiece s = case s of
  Occupied _ -> True
  _ -> False


-- | Check if a supplied square falls within the bounds of normal chess dimensions.
isValidSquare :: Square -> Bool
isValidSquare (x,y) = (x >= 0 &&  x <= 7) && (y >= 0 && y <= 7)


-- | Retrieve all the pieces on the board as a list of pieces.
getAllPieces :: Board -> [Piece]
getAllPieces b = [justPiece (lookupB s b) | s <- [(x,y) | x<-[0..7], y<-[0..7]], isPiece (lookupB s b)]

-- | Makes the board intoa human-readable Text representation
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


-- | Check the top and bottom ranks if we should convert pawns to queens. White pawns get promoted at the 8th rank, while black pawns get promoted at the 1st rank.
evaluatePromotions :: Board -> Board
evaluatePromotions = M.mapWithKey promoteIfPossible
  where
    promoteIfPossible :: Square -> Piece -> Piece
    promoteIfPossible s p = case p of
      Piece Pawn White _ -> if s `elem` ([(x,7)|x<-[0..7]]) then Piece Queen White True else p
      Piece Pawn Black _ -> if s `elem` ([(x,0)|x<-[0..7]]) then Piece Queen Black True else p
      _ -> p


-- | Applies a move to the board if the move is legal. Includes a flag to "force" a move.
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
        let pColor = pieceColor p
        let forceMoveOnBoard = makeMove' m True board
        let isValid = isJust forceMoveOnBoard && S.member (Move p one two) movingPieceMoves
        isValid && not (kingIsInCheck' pColor (fromJust forceMoveOnBoard))
      Empty -> False
      Illegal -> error "Illegal squares passed the initial check."

-- | Make a move on the board. Returns the board with the new move applied to it upon success and Nothing if the move was illegal.
makeMove :: Move -> Board -> Maybe Board
makeMove m = makeMove' m False


-- | Collect the possible moves for a piece in a given position
getMoves :: PositionedPiece -> Board -> S.Set Move
getMoves (p@(Piece pt c _), position) board =
    if pt==Pawn then getPawnMoves (p, position) board else
      let movementPattern = getMovementPattern pt
          deltasList = deltas movementPattern
          isContinuous = continous movementPattern
      in S.unions [followDelta p board position delta c isContinuous | delta <- deltasList]


-- | Export-friendly version of is_legal
isLegalMove :: Move -> Board -> Bool
isLegalMove m b = isJust (makeMove m b)

-- | Return all valid moves for a given board. A valid move is a move that can theoretically be made, meaning that it coincides with the piece's movement. This seperates it from a legal move, which is a valid move that does not leave you in check at the end of your turn. Valid moves and legal moves are usually the same, but not always.
getAllMoves :: Board -> S.Set Move
getAllMoves b = S.union (getAllColorMoves White b) (getAllColorMoves Black b)

-- | Return all legal moves for a given board. A legal move is a valid move that is garuanteed not to leave you in check. This requires additional checks and is therefore more computationally expensive.
getAllLegalMoves :: Board -> S.Set Move
getAllLegalMoves b = S.union (getAllLegalColorMoves White b) (getAllLegalColorMoves Black b)

-- | Return all valid moves for a given board and color. A valid move is a move that can theoretically be made, meaning that it coincides with the piece's movement. This seperates it from a legal move, which is a valid move that does not leave you in check at the end of your turn. Valid moves and legal moves are usually the same, but not always.
getAllColorMoves :: Color -> Board -> S.Set Move
getAllColorMoves c b = S.unions $ [getPieceMoves (x, y) | x <- [0 .. 7], y <- [0 .. 7]]
  where
    getPieceMoves sq = case lookupB sq b of
      Occupied p@(Piece _ colorOfPiece _)| colorOfPiece == c -> getMoves (p,sq) b
      _ -> S.empty

-- | Return all legal moves for a given board and color. A legal move is a valid move that is garuanteed not to leave you in check. This requires additional checks and is therefore more computationally expensive.
getAllLegalColorMoves :: Color -> Board -> S.Set Move
getAllLegalColorMoves c b = S.filter (`isLegalMove` b) (getAllColorMoves c b)

--This is probably a helper function
getNextSquare :: Delta -> Square -> Maybe Square
getNextSquare (rowD, colD) (row, col) = if isValidSquare (row+rowD, col+colD) then Just (row+rowD, col+colD)  else Nothing

-- | Returns the squares around another square as a list
getSurroundings :: Square -> [Square]
getSurroundings s = catMaybes [getNextSquare d s | d <- getCircle]

-- | Function to find the king's square for a given color
getKingSquare :: Color -> Board -> Square
getKingSquare color = fst . head . filter isKing . M.assocs
  where
    isKing (_, piece) = pieceType piece == King && pieceColor piece == color

-- | Checks whether or not the king of a given color is in check by seeing if it's targeted by any of its enemies
kingIsInCheck :: Color -> S.Set Move -> Board -> Bool
kingIsInCheck friendlyColor enemyMoves b = not $ null [lookupB (new_square m) b | m <- S.toList enemyMoves,
            case lookupB (new_square m) b of
                Occupied (Piece King someColor _) -> someColor==friendlyColor
                _ -> False
            ]

kingIsInCheck' :: Color -> Board -> Bool
kingIsInCheck' friendlyColor b = let enemyMoves = getAllColorMoves (oppositeColor friendlyColor) b
  in kingIsInCheck friendlyColor enemyMoves  b 


-- | Checks that after a given move; if the supplied color's king is in check
inCheckAfterMove :: Move -> Color -> Board -> Bool
inCheckAfterMove m myColor b =
    case makeMove m b of
        Just movedBoard -> kingIsInCheck myColor (getAllColorMoves (oppositeColor myColor) movedBoard) movedBoard
        Nothing -> False

-- | Checks whether or not a king can get out of check, meaning that it has legal moves left. Useful for checkmate validation
canGetOutOfCheck :: Color -> Board -> Bool
canGetOutOfCheck c b = or [not $ inCheckAfterMove m c b | m <- S.toList (getAllColorMoves c b)]

-- | Checks whether or not a king has any legal moves. Useful for both checkmate and stalemate evaluations.
hasLegalMoves :: Color -> S.Set Move -> Board -> Bool
hasLegalMoves c friend b = or [not (inCheckAfterMove m c b) | m <- S.toList friend]



-- | "Follows" a delta until it reaches completion, either by going out of bounds or by encountering a piece. See MovementPattern for more details.
followDelta :: Piece -> Board -> Square -> Delta -> Color -> Bool -> S.Set Move
followDelta p board start delta c continuous = go start
  where
    go sq
      | not continuous = case getNextSquare delta sq of
          Just nextSq -> case lookupB nextSq board of
            Illegal -> S.empty --
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


-- | Pawns are unique, in that they capture and move differently. Therefore it's best to handle their movement-logic seperately.
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


-- | Performs a castling action, which involves swapping your king and rook to achieve additional safety.
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
