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


--A piece is defined as a composite type of its piecetype and its color
data Piece = Piece { pieceType :: PieceType, color :: Color, hasMoved :: Bool }
    deriving (Show, Eq)


--Get the starting version of a given colored piece. Allows shortening "Piece pt c False", since all pieces start unmoved
startP :: PieceType -> Color -> Piece 
startP pt c = Piece pt c False

---------------------------------------------------------------------------------------------------------


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

