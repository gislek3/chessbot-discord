module Chess.Piece where



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

type Square = (Int, Int)

--A piece is defined as a composite type of its piecetype and its color
data Piece = Piece { pieceType :: PieceType, color :: Color }
    deriving (Show, Eq)

type PositionedPiece = (Piece, Square)

--A move consists of a Piece making a move and the square it is moving to
data Move = Move {piece :: Piece, old_square :: Square, new_square :: Square}
    deriving (Show, Eq)

type Delta = (Int, Int)

data MovementPattern = MovementPattern {
    deltas :: [Delta],
    continous :: Bool
}

getMovementPattern :: PieceType -> MovementPattern
getMovementPattern pt = case pt of
    Rook -> MovementPattern {deltas=[(-1, 0), (1, 0), (0, 1), (0, 1)], continous=True}
    Bishop -> MovementPattern {deltas=[(-1, 1), (-1, -1), (1, 1), (1, -1)], continous=True}
    Knight -> MovementPattern{deltas=[(-1, 2), (1, 2), (-1, -2), (1, -2), (-2, 1), (-2, -1), (2, -1), (2, 1)], continous=False}
    Queen -> MovementPattern{deltas=[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)], continous=True}
    King -> MovementPattern{deltas=[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)], continous=False}
    _ -> MovementPattern {deltas=[], continous=False}

