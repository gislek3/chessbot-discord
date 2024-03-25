{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Chess.Board where


--Local imports
import Chess.Piece

--Other imports
import qualified Data.Map as M
import Data.Map (lookup)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S


-- Define a Square as a wrapper around (Int, Int)


-- Define the ChessBoard as a map from Square to Maybe Piece
type Board = M.Map Square (Maybe Piece)

{- instance Show Board where
  show = showB -}

-- Initialize an empty chess board
empty :: Board
empty = M.fromList [((i, j), Nothing) | i <- [0..7], j <- [0..7]]

-- Place a piece on the board
place :: (Square, Piece) -> Board -> Board
place (square, piece) = M.insert square (Just piece)

-- Clear a square on the board, does nothing if square is empty
clear :: Square -> Board -> Board
clear square = M.insert square Nothing


-- TODO: Initialize the initial board, in the starting position
startingBoard :: Board
startingBoard = foldr place empty startingPieces
  where
    -- List of pieces with their starting positions
    startingPieces :: [(Square, Piece)]
    startingPieces =
      [ ((0, 0), startP Rook Black), ((7, 0), startP Rook Black),
        ((1, 0), startP Knight Black), ((6, 0), startP Knight Black),
        ((2, 0), startP Bishop Black), ((5, 0), startP Bishop Black),
        ((3, 0), startP Queen Black), ((4, 0), startP King Black)
      ]
      ++ [((x, 1), startP Pawn Black) | x <- [0..7]]
      ++ [((x, 6), startP Pawn White) | x <- [0..7]]
      ++ [ ((0, 7), startP Rook White), ((7, 7), startP Rook White),
           ((1, 7), startP Knight White), ((6, 7), startP Knight White),
           ((2, 7), startP Bishop White), ((5, 7), startP Bishop White),
           ((3, 7), startP Queen White), ((4, 7), startP King White)
      ]


-- Helper function to convert a Piece to its Unicode representation. Change if desired.
-- NOTE: black and white pieces are swapped, it just makes sense
pieceToChar :: Maybe Piece -> Char
pieceToChar p = case p of
    Just (Piece Pawn White _) -> '\x265F'
    Just (Piece Rook White _) -> '\x265C'
    Just (Piece Knight White _) -> '\x265E'
    Just (Piece Bishop White _) -> '\x265D'
    Just (Piece Queen White _) -> '\x265B'
    Just (Piece King White _) -> '\x265A'
    Just (Piece Pawn Black _) -> '\x2659'
    Just (Piece Rook Black _) -> '\x2656'
    Just (Piece Knight Black _) -> '\x2658'
    Just (Piece Bishop Black _) -> '\x2657'
    Just (Piece Queen Black _) -> '\x2655'
    Just (Piece King Black _) -> '\x2654'
    Nothing -> '\x2002'  -- Space for empty square


pieceToChar2 :: Maybe Piece -> Char
pieceToChar2 p = case p of
    Just (Piece Pawn White _) -> 'P'
    Just (Piece Rook White _) -> 'R'
    Just (Piece Knight White _) -> 'N'
    Just (Piece Bishop White _) -> 'B'
    Just (Piece Queen White _) -> 'Q'
    Just (Piece King White _) -> 'K'
    Just (Piece Pawn Black _) -> 'p'
    Just (Piece Rook Black _) -> 'r'
    Just (Piece Knight Black _) -> 'n'
    Just (Piece Bishop Black _) -> 'b'
    Just (Piece Queen Black _) -> 'q'
    Just (Piece King Black _) -> 'k'
    Nothing -> ' '  -- Space for empty square


--Wrapper for Map.lookup that uses Either error handling (to deal with indexing errors), and works around the "double maybe" which results in a lookup on a Map to Maybe a
-- This allows for empty squares to be Nothing and illegal squares to be something else.
lookupB :: Square -> Board -> Either String (Maybe Piece)
lookupB s b
  | isValidSquare s && isNothing (M.lookup s b) = Right Nothing
  | isValidSquare s = Right (fromJust (M.lookup s b))
  | otherwise = Left "Square is out of bounds. For any (x,y) make sure x <- [0..7] and y <- [0..7]."


isValidSquare :: Square -> Bool
isValidSquare (x,y) = (x >= 0 &&  x <= 7) && (y >= 0 && y <= 7)

-- Converts the chess board to a human-readable string representation.
showB :: Board -> T.Text
showB b = T.intercalate "\n" (topMargin : boardRows ++ [bottomMargin])
  where
    topMargin = "  a b c d e f g h"
    bottomMargin = topMargin

    boardRows = [T.pack (show (8 - y)) <> " " <> rowToString y <> " " <> T.pack (show (8 - y)) | y <- [0..7]]

    rowToString :: Int -> T.Text
    rowToString y = T.concat [
        T.singleton (
            case lookupB (x, y) b of
                Left _ -> '_'  --this error can't occur here but we have to handle it
                Right maybePiece -> pieceToChar maybePiece
        ) <> " " | x <- [0..7]]



makeMove :: Move -> Board -> Maybe Board
makeMove Move {old_square=start, new_square=end} board =
  case lookupB start board of
    Left _ -> Nothing  --Illegal coordinates are always illegal moves
    Right Nothing -> Nothing  --No piece at the start square
    Right (Just p) -> --Start square contains a piece, attempt to make move
      --return new board if successful, otherwise Nothing
      makeMove (Move p start end) board



-- Applies a move to the board if the move is legal
makeMove' :: Move -> Board -> Maybe Board
makeMove' move board =
    if is_legal move board then
        let
            boardWithoutOldPiece = clear (old_square move) board
            boardWithNewPiece = place (new_square move, piece move) boardWithoutOldPiece
        in
            Just boardWithNewPiece
    else
        Nothing
  where
    -- Placeholder for the legal move checker, always returns True for now
    is_legal :: Move -> Board -> Bool
    is_legal _ _ = True


--Return all legal moves for a given board
getAllMoves :: Board -> S.Set Move
getAllMoves _ = S.empty

--Return all legal moves for a given piece on a board
getMoves :: PositionedPiece -> Board -> S.Set Move
getMoves (piece@(Piece { pieceType = pt, color = pc, hasMoved = hm }), (row, col)) board =
    let pattern = getMovementPattern pt
        in S.empty

getNextSquare :: Delta -> Square -> Maybe Square
getNextSquare (rowD, colD) (row, col) = if isValidSquare(row+rowD, col+colD) then Just (row+rowD, col+colD)  else Nothing

-- Handle pawn moves separately due to their unique rules
pawnMoves :: Piece -> Square -> Board -> [Move]
pawnMoves piece@(Piece Pawn color hasMoved) square@(row, col) board = [] -- Your pawn specific
