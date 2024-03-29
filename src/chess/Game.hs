module Chess.Game (module Chess.Game) where

import Chess.Board
import Chess.Piece

import qualified Data.Set as S
import Data.Maybe (fromJust, isNothing)


data GameState = Active | Resigned | Drawn | InCheck Color | CheckMate Color | Stalemate deriving (Show, Eq)
data ToPlay = ON Color | OFF deriving (Show, Eq)

-- CommandResult might include the outcome, a message for the user, and optionally the current state of the board.
data ChessGame = ChessGame {
    board :: Board,
    toPlay :: ToPlay,
    playerColor :: Color,
    gameState :: GameState,
    updated :: Bool
} deriving (Show, Eq)


defaultStart :: ChessGame
defaultStart = ChessGame {
    board = startingBoard,
    toPlay = ON White,
    playerColor = White,
    gameState = Active,
    updated = False
}

startBlack :: ChessGame
startBlack = defaultStart{playerColor=Black}

swapPlayer :: ChessGame -> ChessGame
swapPlayer game@(ChessGame { toPlay = ON currColor }) =
    game { toPlay = ON (oppositeColor currColor), updated = True }
swapPlayer game = game {updated = False}

resign :: ChessGame -> ChessGame
resign game = game { toPlay = OFF, gameState = Resigned, updated=True }

draw :: ChessGame -> ChessGame
draw game = game { toPlay = OFF, gameState = Drawn, updated=True  }

getCurrentPlayer :: ChessGame -> Maybe Color
getCurrentPlayer ChessGame{toPlay=tp} = case tp of
    OFF -> Nothing
    ON c -> Just c

evaluateGameState :: ChessGame -> ChessGame
evaluateGameState g@(ChessGame{board=b, gameState=gs, toPlay=tp}) =
  case tp of
    OFF -> g -- Game is off, no changes
    ON currentPlayer -> 
      let allBlackMoves = getAllColorMoves Black b
          allWhiteMoves = getAllColorMoves White b
          whiteIsInCheck = kingIsInCheck White allBlackMoves b
          blackIsInCheck = kingIsInCheck Black allWhiteMoves b
          newState = case currentPlayer of
            White -> if whiteIsInCheck 
                     then if canGetOutOfCheck White b then InCheck White else CheckMate White
                     else gs
            Black -> if blackIsInCheck 
                     then if canGetOutOfCheck Black b then InCheck Black else CheckMate Black
                     else gs
      in g{gameState=newState, toPlay= if newState `elem` [CheckMate White, CheckMate Black] then OFF else ON $ oppositeColor currentPlayer, updated=True}
    

    --let blackKingInCheck = kingIsInCheck Black

    {-
    infer who the current player is
    check if they are putting themselves in check
    check then if they are checking the opposite player
    if they are, figure out if the other player can get out of check or not -- this decides checkmate
    if no player is in check, infer whether or not the opposite player is in statelmate
    
    if 
    -}


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
