module Chess.Game (move, reset, resign, castle, defaultStart, ChessData(..), ChessGame, ToPlay(..), GameState(..), Updated) where

--Imports
import Chess.Board
import Chess.Piece
import Control.Monad.State
import Data.Set()
import Computer.MoveFinder (findBestMove)
import Data.Maybe (fromJust)





{-
A game is a board and its rules. Here, all the "game logic" is handled, which means
that we decide the state of the game based on what is happening on the board and whose
turn it is to play. Using the State monad, I defined this as a composition of data
relating to the chess board, whose turn it is to play, who is controlling which color,
and inferred properties like checks, checkmates and stalemates.
-}


-- | Boolean value indicating whether or not the change to the state was successful.
type Updated = Bool

-- | A game consists of its state and a value indicating whether or not the board was updated. This value is True when an operation stemming from player input resulted in a successful modification to the state, and False when it did not. Illegal moves, improper castling will give failures, resulting in unchanged state and a False value. We may use this boolean value and the gameState to make complex observations about the state of the current game of chess. 
type ChessGame = State ChessData Updated

-- | Various game states. Most normal ones are Active (normal play), Check and Checkmate.
data GameState = Active | Resigned | Drawn | InCheck Color | CheckMate Color | Stalemate | Reset | Failed deriving (Show, Eq)

-- | Type which tracks whose color's turn it is to play. If it's nobody's turn, this will be set to OFF
data ToPlay = ON Color | OFF deriving (Show, Eq)

-- | State type, includes the board, whose turn it is to play, which color the player is controlling, and the inferred state of the game.
data ChessData = ChessData {
    board :: Board,
    toPlay :: ToPlay,
    playerColor :: Color,
    gameState :: GameState
} deriving (Show, Eq)


-- | Default starting data for a normal game of chess.
defaultStart :: ChessData
defaultStart = ChessData {
    board = startingBoard,
    toPlay = ON White,
    playerColor = White,
    gameState = Active
}

-- | Return the default starting ChessData, but for black.
startBlack :: ChessData
startBlack = defaultStart{playerColor=Black}

-- | Helper function that modifies the state and returns a value, indicating successful changes to the state.
update :: ChessData -> ChessGame
update d = do
    put d
    return True

-- | Restart the game, keeping all settings except the board and turn logic.
reset :: ChessGame
reset = get >>= \g -> update g{board=startingBoard, toPlay = ON White, gameState=Active}

-- | Resign from the current game, which ends it.
resign :: ChessGame
resign = get >>= \g -> update g{toPlay = OFF, gameState = Resigned}

-- | End the game for any reason.
off :: ChessGame
off = get >>= \g -> update g{toPlay = OFF}

-- | Set the game to be drawn, which ends it.
draw :: ChessGame
draw = get >>= \g -> update g{toPlay = OFF, gameState = Drawn}



--TODO: delete this
-- | Get the color of the current player, if possible.
getPlayer :: ChessData -> Maybe Color
getPlayer d = case toPlay d of
    ON c -> Just c
    OFF -> Nothing



-- | Wrapper function for movement, which is needed as Games' care about state-conditionals like "toPlay". For movement logic, see the relevant function in Board.hs.
move :: Square -> Square -> ChessGame
move start end = do
    g <- get
    if toPlay g == OFF then return False else do
        let p = lookupB start (board g)
        if not (isPiece p) || (fromJust (getPlayer g) /= pieceColor (justPiece p)) then return False else
            case makeMove (Move (justPiece p) start end) (board g) of
                Nothing -> return False
                Just movedBoard -> do
                    put g{board=movedBoard}
                    updateGameState



-- | Changes whose turn it is. If it's the computer to play, get a response before looping back, otherwise update the state and return value.
swap :: ChessGame
swap = get >>= \g -> case toPlay g of
    OFF -> return False
    ON c -> if c==playerColor g then do
                put g{toPlay=ON $ oppositeColor c}
                respond
            else do
                put g{toPlay=ON $ oppositeColor c}
                return True



-- | Get a move in response to whatever the board's state is. For how the actual move is decided, see the relevant sections of the Computer module. 
respond :: ChessGame
respond = get >>= \g -> do
    if toPlay g == OFF then return False else
        case findBestMove (board g) (oppositeColor $ playerColor g) of
            Just (Move _ start end) -> move start end
            Nothing -> return False



-- | Wrapper function for castling actions in Game. For actual castling logic, see the relevant function in Board.hs
castle :: PieceType -> ChessGame
castle pt = get >>= \g ->
    if toPlay g == OFF then return False
    else if notElem pt [King, Queen] then return False else
        case castleKing pt (playerColor g) (board g) of
            Nothing -> return False
            Just b -> do
                put g{board=b}
                swap



-- | Routes the decision-making based on the game's state after an evaluation of its data.
updateGameState :: ChessGame
updateGameState = get >>= \g -> do
    let newG = evaluateGameState g
    put newG
    case gameState newG of 
        CheckMate _ -> off
        Stalemate -> off
        Drawn -> off
        Active -> swap
        _ -> swap

-- | Evaluates a ChessData, leaving it unaltered apart from modifying its state. An evaluation consists of logical checks to infer checkmates, checks, stalemates, draws, etc.
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
