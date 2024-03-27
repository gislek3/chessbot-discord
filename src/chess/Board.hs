{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Chess.Board where


--Local imports
import Chess.Piece
    ( Delta,
      Piece(Piece, color),
      PieceType(Pawn, Rook, Knight, Bishop, Queen, King),
      Color(..),
      startP )

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



-- Helper function to convert a Piece to its Unicode representation. Change if desired.
-- NOTE: black and white pieces are swapped, it just makes sense
pieceToChar :: Piece -> Char
pieceToChar p = case p of
    Piece Pawn White _ -> '\x265F'
    (Piece Rook White _) -> '\x265C'
    (Piece Knight White _) -> '\x265E'
    (Piece Bishop White _) -> '\x265D'
    (Piece Queen White _) -> '\x265B'
    (Piece King White _) -> '\x265A'
    (Piece Pawn Black _) -> '\x2659'
    (Piece Rook Black _) -> '\x2656'
    (Piece Knight Black _) -> '\x2658'
    (Piece Bishop Black _) -> '\x2657'
    (Piece Queen Black _) -> '\x2655'
    (Piece King Black _) -> '\x2654'



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
showB b = T.intercalate "\n" (topMargin : boardRows ++ [bottomMargin])
  where
    topMargin = "  a b c d e f g h"
    bottomMargin = topMargin

    -- Generate rows starting from the bottom (7 to 0)
    boardRows = [T.pack (show (y+1)) <> " " <> rowToString y <> " " <> T.pack (show (y+1)) | y <- [7,6..0]]

    rowToString :: Int -> T.Text
    rowToString y = T.concat [
        T.singleton (
            case lookupB (x, y) b of
                Illegal -> error "Illegal square found when trying to show the board."
                Empty -> '_'
                Occupied somePiece -> pieceToChar somePiece
        ) <> " " | x <- [0..7]]



makeMove :: Move -> Board -> Maybe Board
makeMove Move {old_square=start, new_square=end} board =
  case lookupB start board of
    Empty -> Nothing  --No piece at the start square
    Occupied p -> --Start square contains a piece, attempt to make move
      --return new board if successful, otherwise Nothing
      makeMove' (Move p start end) board
    _ -> Nothing --Illegal coordinates and other things are always illegal moves



-- Applies a move to the board if the move is legal
makeMove' :: Move -> Board -> Maybe Board
makeMove' move board =
    if (isValidSquare' move) && (is_legal move board) then
        let
            boardWithoutOldPiece = clear (old_square move) board
            boardWithNewPiece = place (new_square move) (piece move) boardWithoutOldPiece
        in
            Just boardWithNewPiece
    else
        Nothing
  where
    -- Placeholder for the legal move checker, always returns True for now
    --should simply check that 
    is_legal :: Move -> Board -> Bool
    is_legal _ _ = True


--Return all legal moves for a given board
getAllMoves :: Board -> S.Set Move
getAllMoves _ = S.empty

--Return all legal moves for a given piece on a board
getMoves :: Piece -> Board -> S.Set Move
getMoves _ _ = S.empty

getNextSquare :: Delta -> Square -> Maybe Square
getNextSquare (rowD, colD) (row, col) = if isValidSquare (row+rowD, col+colD) then Just (row+rowD, col+colD)  else Nothing

