module Chess.Piece (module Chess.Piece) where


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
data Piece = Piece { pieceType :: PieceType, pieceColor :: Color, hasMoved :: Bool }
    deriving (Show, Eq)


--Get the starting version of a given colored piece. Allows shortening "Piece pt c False", since all pieces start unmoved
startP :: PieceType -> Color -> Piece 
startP pt c = Piece pt c False

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

---------------------------------------------------------------------------------------------------------

--Delta
type Delta = (Int, Int)


data MovementPattern = MovementPattern {
    deltas :: [Delta],
    continous :: Bool
}

getPawnMovement :: Color -> Bool -> [Delta]
getPawnMovement c hm = case c of
    White -> if hm then [(0, 1)] else [(0,1),(0,2)]
    Black -> if hm then [(0, -1)] else [(0,-1),(0,-2)]

getCircle :: [Delta]
getCircle = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

getMovementPattern :: PieceType -> MovementPattern
getMovementPattern pt = case pt of
    Rook -> MovementPattern {deltas=[(-1, 0), (1, 0), (0, -1), (0, 1)], continous=True}
    Bishop -> MovementPattern {deltas=[(-1, 1), (-1, -1), (1, 1), (1, -1)], continous=True}
    Knight -> MovementPattern{deltas=[(-1, 2), (1, 2), (-1, -2), (1, -2), (-2, 1), (-2, -1), (2, -1), (2, 1)], continous=False}
    Queen -> MovementPattern{deltas=[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)], continous=True}
    King -> MovementPattern{deltas=[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)], continous=False}
    Pawn -> MovementPattern{deltas=[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)], continous=False}
    --Pawn -> error "getMovementPattern called with pawn as argument, please use ????? instead."

