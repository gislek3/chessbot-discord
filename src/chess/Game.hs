module Chess.Game (module Chess.Game) where

import Chess.Board
import Chess.Piece

import qualified Data.Set as S
import Data.Maybe (fromJust, isNothing)


data GameState = Active | Resigned | Drawn | InCheck Color | CheckMate Color | Stalemate | Reset deriving (Show, Eq)
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

swap :: ChessGame -> ChessGame
swap g = case toPlay g of
    OFF -> same g
    ON c -> g{toPlay=ON $ oppositeColor c, updated=True}

same :: ChessGame -> ChessGame
same g = g{updated=False}

reset :: ChessGame -> ChessGame
reset g = g{board=startingBoard, toPlay = ON White, gameState=Active, updated=True}

startBlack :: ChessGame
startBlack = defaultStart{playerColor=Black}

resign :: ChessGame -> ChessGame
resign game = game { toPlay = OFF, gameState = Resigned, updated=True }

off :: ChessGame -> ChessGame
off game = game{toPlay = OFF, updated=True}

draw :: ChessGame -> ChessGame
draw game = game { toPlay = OFF, gameState = Drawn, updated=True  }

getCurrentPlayer :: ChessGame -> Maybe Color
getCurrentPlayer ChessGame{toPlay=tp} = case tp of
    OFF -> Nothing
    ON c -> Just c

evaluateGameState :: ChessGame -> ChessGame
evaluateGameState g@(ChessGame{board=b, gameState=gs}) =
    let allBlackMoves = getAllColorMoves Black b
        allWhiteMoves = getAllColorMoves White b
        whiteIsInCheck = kingIsInCheck White allBlackMoves b
        blackIsInCheck = kingIsInCheck Black allWhiteMoves b
        newState = case toPlay g of
            OFF -> gs
            ON White -> if whiteIsInCheck then InCheck White else
                     if blackIsInCheck && not (canGetOutOfCheck Black b) then CheckMate Black else
                     if blackIsInCheck then InCheck Black else if null allWhiteMoves then Stalemate else gs
            ON Black -> if blackIsInCheck then InCheck Black else
                     if whiteIsInCheck && not (canGetOutOfCheck White b) then CheckMate White else
                     if whiteIsInCheck then InCheck White else if null allWhiteMoves then Stalemate else gs
      in g{gameState=newState}


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
kingIsInCheck friendlyColor enemyMoves board = not $ null [lookupB (new_square m) board | m <- S.toList enemyMoves,
            case lookupB (new_square m) board of
                Occupied (Piece King someColor _) -> someColor==friendlyColor
                _ -> False
            ]

kingIsInCheckDebug :: Color -> S.Set Move -> Board -> [SquareContent]
kingIsInCheckDebug friendlyColor enemyMoves board = [lookupB (new_square m) board | m <- S.toList enemyMoves,
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
move start end g@(ChessGame{board=b, toPlay=tp}) = case lookupB start b of
    Illegal -> g{updated=False}
    Empty -> g{updated=False}
    Occupied occupiedPiece -> case tp of
        OFF -> g{updated=False}
        ON gc -> if gc==pieceColor occupiedPiece then move' (Move occupiedPiece start end) g else g{updated=False}

move' :: Move -> ChessGame -> ChessGame
move' m g@(ChessGame {board = b, toPlay=ON cp}) = case makeMove m b of
    Nothing -> g {updated = False}
    Just movedBoard -> do
        let new = evaluateGameState $ g{board = movedBoard}
        case gameState new of
            InCheck gc -> if gc==cp then same new else swap new
            CheckMate _ -> off new
            Stalemate -> off new
            Active -> swap new
            _ -> swap new
move' _ g@(ChessGame _ OFF _ _ _) = same g
