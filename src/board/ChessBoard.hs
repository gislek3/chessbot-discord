{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Board.ChessBoard where


import qualified Data.Map as M
import Data.Map (lookup)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


-- Define the Color of pieces
data Color = White | Black
    deriving (Show, Eq)

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
emptyBoard :: Board
emptyBoard = M.fromList [((i, j), Nothing) | i <- [0..7], j <- [0..7]]

-- Place a piece on the board
placePiece :: (Square, Piece) -> Board -> Board
placePiece (square, piece) board = M.insert square (Just piece) board

-- TODO: Initialize the initial board, in the starting position
startingBoard :: Board
startingBoard = foldr placePiece emptyBoard startingPieces
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
pieceToChar :: Maybe Piece -> Char
pieceToChar p = case p of
    Just (Piece Pawn White) -> '\x2659'
    Just (Piece Rook White) -> '\x2656'
    Just (Piece Knight White) -> '\x2658'
    Just (Piece Bishop White) -> '\x2657'
    Just (Piece Queen White) -> '\x2655'
    Just (Piece King White) -> '\x2654'
    Just (Piece Pawn Black) -> '\x265F' 
    Just (Piece Rook Black) -> '\x265C'
    Just (Piece Knight Black) -> '\x265E'
    Just (Piece Bishop Black) -> '\x265D'
    Just (Piece Queen Black) -> '\x265B'
    Just (Piece King Black) -> '\x265A'
    Nothing -> '\x2002'


--Wrapper for Map.lookup
lookupSquare :: Square -> Board -> Maybe Piece
lookupSquare s b = case (M.lookup s b) of
    Just (Just a) -> Just a
    Just Nothing -> Nothing
    Nothing -> error "Square is out of bounds. For any (x,y) make sure x <- [0..7] and y <- [0..7]."


-- Converts the chess board to a human-readable string representation.
showBoard :: Board -> T.Text
showBoard b = T.intercalate "\n" (topMargin : boardRows ++ [bottomMargin])
  where
    -- Margine with file labels (a to h) for the chess board.
    topMargin = "  a b c d e f g h"
    bottomMargin = topMargin

    boardRows = [T.pack (show (8-y)) <> " "    -- Convert the row index to a rank label, add to beginning
                 <> rowToString y              -- Generate a string representation of the row.
                 <> " " <> T.pack (show (8-y)) -- Add the same rank label to the end.
                 | y <- [0..7]] -- Do this for each row


    rowToString y = T.concat [T.singleton (pieceToChar (lookupSquare (x, y) b)) <> " "
                              | x <- [0..7]] -- see helper function pieceToChar


-- Applies a move to the board if the move is legal
makeMove :: Move -> Board -> Maybe Board
makeMove move board =
    if is_legal move board then
        let 
            boardWithoutOldPiece = M.insert (old_square move) Nothing board
            boardWithNewPiece = M.insert (new_square move) (Just (piece move)) boardWithoutOldPiece
        in 
            Just boardWithNewPiece
    else
        Nothing
  where
    -- Placeholder for the legal move checker, always returns True for now
    is_legal :: Move -> Board -> Bool
    is_legal _ _ = True

