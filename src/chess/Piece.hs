module Chess.Piece where

import Chess.Board (Square)


{-
TODO: before fixing this, try to correct the board,
because now e2 e4 is upside down
need to figure that shit out first before I can do move logic
-}


--Pieces can either be black or white
data Color = White | Black deriving (Show, Eq)

--Standard pieces
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Eq)

--A piece is defined as a composite type of its piecetype and its color
data Piece = Piece { pieceType :: PieceType, color :: Color }
    deriving (Show, Eq)

--A move consists of a Piece making a move and the square it is moving to
data Move = Move {piece :: Piece, old_square :: Square, new_square :: Square}
    deriving (Show, Eq)

type Delta = (Int, Int)

data MovementPattern = MovementPattern {
    deltas :: [Delta]
    continous :: Bool
}

getMovementPattern :: PieceType -> MovementPattern
getMovementPattern = case pt of
    Rook -> MovementPattern {deltas=[(-1, 0), (1, 0), [0, 1], (0, 1)], continous=True}
    Bishop -> MovementPattern {deltas=[(-1, 1), (-1, -1), [1, 1], (1, -1)], continous=True}
    _ -> MovementPattern {deltas=[], continous=False}

--Return all legal moves for a given board
getAllMoves :: Board -> Set Move
getAllMoves _ = Set.empty

--Return all legal moves for a given piece on a board
getMoves :: Piece -> Board -> Set Move
getMoves _ _ = Set.empty



--TODO: make recursive movescanner that just takes in a square, pattern, board, and searches
