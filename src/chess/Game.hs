module Chess.Game (module Chess.Game) where

import Chess.Board
import Chess.Piece

import qualified Data.Set as S
import Data.Maybe (fromJust)


data GameState = Resigned | Drawn | Active | InCheck Color | Stalemate deriving (Show, Eq)
data ToPlay = ON Color | OFF deriving (Show, Eq)

-- CommandResult might include the outcome, a message for the user, and optionally the current state of the board.
data ChessGame = ChessGame {
    board :: Board,
    toPlay :: ToPlay,
    playerColor :: Color,
    botColor :: Color,
    gameState :: GameState,  --
    updated :: Bool
} deriving (Show, Eq)


defaultStart :: ChessGame
defaultStart = ChessGame {
    board = startingBoard,
    toPlay = ON White,
    playerColor = White,
    botColor = Black,
    gameState = Active,
    updated = False
}

swapPlayer :: ChessGame -> ChessGame
swapPlayer game@(ChessGame { toPlay = ON currColor }) =
    game { toPlay = ON (oppositeColor currColor), updated = True }
swapPlayer game = game {updated = False}

resign :: ChessGame -> ChessGame
resign game = game { toPlay = OFF, gameState = Resigned, updated=True }

draw :: ChessGame -> ChessGame
draw game = game { toPlay = OFF, gameState = Drawn, updated=True  }

{- evaluateGameState :: ChessGame -> GameState
evaluateGameState ChessGame{board=b, gameState=gs} = do
    allBlackMoves <- S.toList getAllColorMoves Black b
    allWhiteMoves <- S.toList getAllColorMoves White b
    gs -}


--Checks whether or not the king of a given color is in check by seeing if it's targeted by any of its enemies
kingIsInCheck :: Color -> S.Set Move -> Board -> Bool
kingIsInCheck friendlyColor enemyMoves board = null [lookupB (new_square m) board | m <- S.toList enemyMoves,
            case lookupB (new_square m) board of
                Occupied (Piece King someColor _) -> someColor==friendlyColor
                _ -> False
            ]


--Given a moveset and a target square, return a subset containing the moves who end up at the target
movesThatReachSquare :: S.Set Move -> Square -> S.Set Move
movesThatReachSquare moves target = S.fromList [m | m <- S.toList moves, new_square m == target]

--Checks that after a given move; if the supplied color's king is in check
inCheckAfterMove :: Move -> Color -> Board -> Bool
inCheckAfterMove m myColor b =
    case makeMove m b of
        Just movedBoard -> kingIsInCheck myColor (getAllColorMoves (oppositeColor myColor) movedBoard) movedBoard
        Nothing -> False

canGetOutOfCheck :: Color -> Board -> Bool
canGetOutOfCheck c b = or [not $ inCheckAfterMove m c b | m <- S.toList (getAllColorMoves c b)]


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
