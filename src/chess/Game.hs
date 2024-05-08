{-# LANGUAGE BlockArguments #-}
module Chess.Game (module Chess.Game) where

import Chess.Board
import Chess.Piece
import Computer.MoveFinder
import Data.Maybe (isJust, fromJust)
import Control.Monad.State
import Debug.Trace
import Debug.Trace
import qualified Data.Set as S
import Chess.Piece (oppositeColor)




type Updated = Bool
type ChessGame = State ChessData Updated
data GameState = Active | Resigned | Drawn | InCheck Color | CheckMate Color | Stalemate | Reset | Failed deriving (Show, Eq)
data ToPlay = ON Color | OFF deriving (Show, Eq)

data ChessData = ChessData {
    board :: Board,
    toPlay :: ToPlay,
    playerColor :: Color,
    gameState :: GameState
} deriving (Show, Eq)


defaultStart :: ChessData
defaultStart = ChessData {
    board = startingBoard,
    toPlay = ON White,
    playerColor = White,
    gameState = Active
}

startBlack :: ChessData
startBlack = defaultStart{playerColor=Black}

swap :: ChessGame
swap = get >>= \g -> do
    trace "Swap has been called" $ return ()
    trace "Swap has been called" $ return ()
    trace "Swap has been called" $ return ()
    trace "Swap has been called????" $ return ()
    case toPlay g of
        OFF -> return False
        ON c -> if c==playerColor g then do
                    trace "Calling for the bot to respond" $ return ()
                    put g{toPlay=ON $ oppositeColor c}
                    respond
                else do
                    trace "Calling for the human to respond" $ return ()
                    put g{toPlay=ON $ oppositeColor c}
                    return True



respond :: ChessGame
respond = get >>= \g -> do
    if toPlay g == OFF then return False else
        case findBestMove (board g) (oppositeColor $ playerColor g) of
            Just (Move _ start end) -> do
                trace "Bot is responding" $ return ()
                move start end
            Nothing -> return False


update :: ChessData -> ChessGame
update d = do
    put d
    return True

reset :: ChessGame
reset = get >>= \g -> update g{board=startingBoard, toPlay = ON White, gameState=Active}

resign :: ChessGame
resign = get >>= \g -> update g{toPlay = OFF, gameState = Resigned}

off :: ChessGame
off = get >>= \g -> update g{toPlay = OFF}

draw :: ChessGame
draw = get >>= \g -> update g{toPlay = OFF, gameState = Drawn}

--Castling wrapper
castle :: PieceType -> ChessGame
castle pt = get >>= \g ->
    if toPlay g == OFF then return False
    else if notElem pt [King, Queen] then return False else
        case castleKing pt (playerColor g) (board g) of
            Nothing -> return False
            Just b -> do
                put g{board=b}
                swap

getPlayer :: ChessData -> Maybe Color
getPlayer d = case toPlay d of
    ON c -> Just c
    OFF -> Nothing

move :: Square -> Square -> ChessGame
move start end = do
    trace ("Trying to make a move in Game:" ++ show start ++ " " ++ show end) $ return ()
    g <- get
    if toPlay g == OFF then return False else do
        let p = lookupB start (board g)
        if not (isPiece p) || (fromJust (getPlayer g) /= pieceColor (justPiece p)) then return False else
            case makeMove (Move (justPiece p) start end) (board g) of
                Nothing -> do
                    trace "Move was NOT valid, returning False" $ return ()
                    return False
                Just movedBoard -> do
                    trace "Move was valid, calling updateGameState" $ return ()
                    put g{board=movedBoard}
                    updateGameState


move' :: Move -> ChessGame
move' (Move _ a b) = move a b


updateGameState :: ChessGame
updateGameState = do
    trace "updating game state?" $ return ()
    g <- get
    let newG = evaluateGameState g
    put newG
    case gameState newG of 
        CheckMate _ -> off
        Stalemate -> off
        Drawn -> off
        Active -> swap
        _ -> swap


evaluateGameState :: ChessData -> ChessData
evaluateGameState g@(ChessData{board=b, gameState=gs}) =
    let allBlackMoves = getAllColorMoves Black b
        allWhiteMoves = getAllColorMoves White b

        whiteIsInCheck = kingIsInCheck White allBlackMoves b
        whiteCantMove = not $ hasLegalMoves White allWhiteMoves b

        blackIsInCheck = kingIsInCheck Black allWhiteMoves b
        blackCantMove = not $ hasLegalMoves Black allBlackMoves b

        newState = case toPlay g of
            OFF -> gs
            ON White -> if whiteIsInCheck then InCheck White else
                     if blackIsInCheck && not (canGetOutOfCheck Black b) then CheckMate Black else
                     if blackIsInCheck then InCheck Black else if blackCantMove then Stalemate else Active
            ON Black -> if blackIsInCheck then InCheck Black else
                     if whiteIsInCheck && not (canGetOutOfCheck White b) then CheckMate White else
                     if whiteIsInCheck then InCheck White else if whiteCantMove then Stalemate else Active
      in g{gameState=newState}
