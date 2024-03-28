module Chess.Game where

import Chess.Board
import Chess.Piece
import Data.Maybe (isJust)


data GameState = Resigned | Drawn | Active deriving (Show, Eq)
data ToPlay = ON Color | OFF deriving (Show, Eq)

-- CommandResult might include the outcome, a message for the user, and optionally the current state of the board.
data ChessGame = ChessGame {
    board :: Board,
    toPlay :: ToPlay,
    playerColor :: Color,
    botColor :: Color,
    state :: GameState,  --
    updated :: Bool
} deriving (Show, Eq)


defaultStart :: ChessGame
defaultStart = ChessGame {
    board = startingBoard,
    toPlay = ON White,
    playerColor = White,
    botColor = Black,
    state = Active,
    updated = False
}

swapPlayer :: ChessGame -> ChessGame
swapPlayer game@(ChessGame { toPlay = ON currColor }) =
    game { toPlay = ON (oppositeColor currColor), updated = True }
swapPlayer game = game {updated = False}

resign :: ChessGame -> ChessGame
resign game = game { toPlay = OFF, state = Resigned, updated=True }

draw :: ChessGame -> ChessGame
draw game = game { toPlay = OFF, state = Drawn, updated=True  }

--TODO: Should probably limit this function to care about whose turn it is also?
move :: Square -> Square -> ChessGame -> ChessGame
move start end g@(ChessGame{board=b}) = case lookupB start b of
        Illegal -> g{updated=False}
        Empty -> g{updated=False}
        Occupied occupiedPiece -> move' (Move occupiedPiece start end) g

move' :: Move -> ChessGame -> ChessGame
move' m@(Move {piece = Piece {pieceColor = pc}}) g@(ChessGame {board = b, toPlay = ON gc})
    | gc == pc = case makeMove m b of
        Just movedBoard -> g {board = movedBoard, toPlay = ON (oppositeColor gc), updated = True}
        Nothing -> g {updated = False}
    | otherwise = g {updated = False}
move' _ g = g {updated = False}
