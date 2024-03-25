module Chess.Piece where



{-
TODO: before fixing this, try to correct the board,
because now e2 e4 is upside down
need to figure that shit out first before I can do move logic
-}


--Pieces can either be black or white
data Color = White | Black deriving (Show, Eq)

oppositeColor :: Color -> Color
oppositeColor c = if c==White then Black else White


--Standard pieces
data PieceType = Knight | Bishop | Rook | Queen | Pawn | King
    deriving (Show, Eq)

type Square = (Int, Int)

--A piece is defined as a composite type of its piecetype and its color
data Piece = Piece { pieceType :: PieceType, color :: Color, hasMoved :: Bool }
    deriving (Show, Eq)

type PositionedPiece = (Piece, Square)

--Get the starting version of a given colored piece. Allows shortening "Piece pt c False", since all pieces start unmoved
startP :: PieceType -> Color -> Piece 
startP pt c = Piece pt c False

---------------------------------------------------------------------------------------------------------


--A move consists of a Piece making a move and the square it is moving to
data Move = Move {piece :: Piece, old_square :: Square, new_square :: Square}
    deriving (Show, Eq)

instance Ord Move where
    compare (Move _ o1 n1) (Move _ o2 n2) = compare (o1, n1) (o2, n2)

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
    Pawn -> error "getMovementPattern called with pawn as argument, please use ????? instead."

