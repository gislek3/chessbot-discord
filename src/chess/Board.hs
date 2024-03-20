{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Chess.Board where

{-
TODO: divide into multiple files.

I only want board logic here, then piece/move logic can be elsehwere
-}


import qualified Data.Map as M
import Data.Map (lookup)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


-- Define the Color of pieces
data Color = White | Black deriving (Show, Eq)

-- Define the Type of pieces
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Eq)

-- Define a Piece as a combination of PieceType and Color
data Piece = Piece { pieceType :: PieceType, color :: Color }
    deriving (Show, Eq)

-- Define a Square as a wrapper around (Int, Int)
type Square = (Int, Int)

-- Define the ChessBoard as a map from Square to Maybe Piece
type Board = M.Map Square (Maybe Piece)

-- A move consists of a Piece making a move and the square it is moving to
data Move = Move {piece :: Piece, old_square :: Square, new_square :: Square}
    deriving (Show, Eq)

-- Initialize an empty chess board
empty :: Board
empty = M.fromList [((i, j), Nothing) | i <- [0..7], j <- [0..7]]

-- Place a piece on the board
place :: (Square, Piece) -> Board -> Board
place (square, piece) board = M.insert square (Just piece) board

-- Clear a square on the board, does nothing if square is empty
clear :: Square -> Board -> Board
clear square board = M.insert square Nothing board

-- TODO: Initialize the initial board, in the starting position
startingBoard :: Board
startingBoard = foldr place empty startingPieces
  where
    -- List of pieces with their starting positions
    startingPieces :: [(Square, Piece)]
    startingPieces = 
      [ ((0, 0), Piece Rook Black), ((7, 0), Piece Rook Black),
        ((1, 0), Piece Knight Black), ((6, 0), Piece Knight Black),
        ((2, 0), Piece Bishop Black), ((5, 0), Piece Bishop Black),
        ((3, 0), Piece Queen Black), ((4, 0), Piece King Black)
      ]
      ++ [((x, 1), Piece Pawn Black) | x <- [0..7]]
      ++ [((x, 6), Piece Pawn White) | x <- [0..7]]
      ++ [  ((0, 7), Piece Rook White), ((7, 7), Piece Rook White),
            ((1, 7), Piece Knight White), ((6, 7), Piece Knight White),
            ((2, 7), Piece Bishop White), ((5, 7), Piece Bishop White),
            ((3, 7), Piece Queen White), ((4, 7), Piece King White)
      ]


-- Helper function to convert a Piece to its Unicode representation. Change if desired.
-- NOTE: black and white pieces are swapped, it just makes sense
pieceToChar :: Maybe Piece -> Char
pieceToChar p = case p of
    Just (Piece Pawn White) -> '\x265F' 
    Just (Piece Rook White) -> '\x265C' 
    Just (Piece Knight White) -> '\x265E' 
    Just (Piece Bishop White) -> '\x265D' 
    Just (Piece Queen White) -> '\x265B' 
    Just (Piece King White) -> '\x265A' 
    Just (Piece Pawn Black) -> '\x2659' 
    Just (Piece Rook Black) -> '\x2656' 
    Just (Piece Knight Black) -> '\x2658' 
    Just (Piece Bishop Black) -> '\x2657' 
    Just (Piece Queen Black) -> '\x2655' 
    Just (Piece King Black) -> '\x2654' 
    Nothing -> '\x2002'  -- Space for empty square


pieceToChar2 :: Maybe Piece -> Char
pieceToChar2 p = case p of
    Just (Piece Pawn White) -> 'P' 
    Just (Piece Rook White) -> 'R' 
    Just (Piece Knight White) -> 'N' 
    Just (Piece Bishop White) -> 'B' 
    Just (Piece Queen White) -> 'Q' 
    Just (Piece King White) -> 'K' 
    Just (Piece Pawn Black) -> 'p' 
    Just (Piece Rook Black) -> 'r' 
    Just (Piece Knight Black) -> 'n' 
    Just (Piece Bishop Black) -> 'b' 
    Just (Piece Queen Black) -> 'q' 
    Just (Piece King Black) -> 'k' 
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
                Right maybePiece -> pieceToChar2 maybePiece
        ) <> " " | x <- [0..7]]



makeMove' :: Move -> Board -> Maybe Board
makeMove' Move {old_square=start, new_square=end} board =
  case lookupB start board of
    Left _ -> Nothing  --Illegal coordinates are always illegal moves
    Right Nothing -> Nothing  --No piece at the start square
    Right (Just p) -> --Start square contains a piece, attempt to make move
      --return new board if successful, otherwise Nothing
      makeMove (Move p start end) board



-- Applies a move to the board if the move is legal
makeMove :: Move -> Board -> Maybe Board
makeMove move board =
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

