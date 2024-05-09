module Chess.Piece (module Chess.Piece) where

{-
Module relating to pieces and their movements.

A piece is a composition of its type and its color. I have an extra field variable,
which is a bool that tracks whether or not the piece has moved. This is very useful,
as pawns can move twice on their first turn, and castling requires both the king and
rook to be unmoved. Since it has limited use, it's a bit bloated, and maybe I should
have found a smarter way to do it (like inferring whether or not a piece has moved
by keeping track of all moves made in the game, but then I would have to again distinguish
between indiviudal rooks and pawns somehow, which might introduce another variable anyway,
so this is what I ended up doing.
-}

-- | A piece is defined as a composite type of its piecetype and its color
data Piece = Piece { pieceType :: PieceType, pieceColor :: Color, hasMoved :: Bool }
    deriving (Show, Eq)


-- | Enumeration type for the two types of colors available in chess, White and Black.
data Color = White | Black deriving (Show, Eq)

-- | Returns White if given Black, and Black if given White
oppositeColor :: Color -> Color
oppositeColor c = if c==White then Black else White


-- | Simple enumeration type for different pieces
data PieceType = Knight | Bishop | Rook | Queen | Pawn | King
    deriving (Show, Eq)


-- | Get the starting version of a given colored piece. Allows shortening "Piece pt c False", since all pieces start unmoved
startP :: PieceType -> Color -> Piece 
startP pt c = Piece pt c False


-- NOTE: black and white pieces are swapped (this makes sense with Discord's dark theme, but makes no sense with its light theme)
-- | Helper function to convert a Piece to its Unicode representation. Change if desired.
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


-- | A delta consists of rank movement, and file movement.
type Delta = (Int, Int)

-- | The pattern in which a piece moves, a list of various deltas, and whether or not the pattern repeats.
data MovementPattern = MovementPattern {
    deltas :: [Delta],
    continous :: Bool
}

-- | Pawns are the only pieces that are not omnidirectional, and also the only pieces which move and attack differently, I therefore need to seperate their movement logic.
getPawnMovement :: Color -> Bool -> [Delta]
getPawnMovement c hm = case c of
    White -> if hm then [(0, 1)] else [(0,1),(0,2)]
    Black -> if hm then [(0, -1)] else [(0,-1),(0,-2)]

-- | Get deltas that form a "circle" / octagon. Useful for getting the "sorroundings" of a square.
getCircle :: [Delta]
getCircle = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]


-- | Get the movement pattern for a piece. Useful when "collecting" moves on a board. See Board.hs for how this works in practice.
getMovementPattern :: PieceType -> MovementPattern
getMovementPattern pt = case pt of
    Rook -> MovementPattern {deltas=[(-1, 0), (1, 0), (0, -1), (0, 1)], continous=True}
    Bishop -> MovementPattern {deltas=[(-1, 1), (-1, -1), (1, 1), (1, -1)], continous=True}
    Knight -> MovementPattern{deltas=[(-1, 2), (1, 2), (-1, -2), (1, -2), (-2, 1), (-2, -1), (2, -1), (2, 1)], continous=False}
    Queen -> MovementPattern{deltas=[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)], continous=True}
    King -> MovementPattern{deltas=[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)], continous=False}
    Pawn -> MovementPattern{deltas=[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)], continous=False}
    --Pawn -> error "getMovementPattern called with pawn as argument, please use ????? instead."

