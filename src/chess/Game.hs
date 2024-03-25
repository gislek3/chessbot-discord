module Chess.Game where

import Chess.Board
import Chess.Piece
import Data.Maybe (isJust)


data GameState = Resign | Draw | Active deriving (Show, Eq)
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
swapPlayer game@(ChessGame { toPlay = ON color }) =
    game { toPlay = ON (oppositeColor color), updated=True }
swapPlayer game = game {updated=False} --Don't swap if game is inactive

resign :: ChessGame -> ChessGame
resign game = game { toPlay = OFF, state = Resign, updated=True }

draw :: ChessGame -> ChessGame
draw game = game { toPlay = OFF, state = Draw, updated=True  }

move :: Square -> Square -> ChessGame -> ChessGame
move start end g@(ChessGame{board=b}) = case lookupB start b of
        Left _ -> g{updated=False}
        Right Nothing -> g{updated=False}
        Right (Just piece) -> move' (Move {piece=piece, old_square=start, new_square=end}) g

move' :: Move -> ChessGame -> ChessGame
move' m@(Move {piece = Piece {color = pc}}) g@(ChessGame {board = b, toPlay = ON gc})
    | gc == pc = case makeMove m b of
        Just movedBoard -> g {board = movedBoard, toPlay = ON (oppositeColor gc), updated = True}
        Nothing -> g {updated = False}
    | otherwise = g {updated = False}
move' _ g = g {updated = False}

oppositeColor :: Color -> Color
oppositeColor c = if c==White then Black else White