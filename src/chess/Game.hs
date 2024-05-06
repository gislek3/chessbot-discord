module Chess.Game (module Chess.Game) where

import Chess.Board
import Chess.Piece
import Computer.MoveFinder
import Data.Maybe (isJust, fromJust)

import qualified Data.Set as S


data GameState = Active | Resigned | Drawn | InCheck Color | CheckMate Color | Stalemate | Reset | Failed deriving (Show, Eq)
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
    ON c -> if c==playerColor g then
            respond g{toPlay=ON $ oppositeColor c}
        else
            g{toPlay=ON $ oppositeColor c, updated=True}

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



--Castling wrapper
castle :: PieceType -> ChessGame -> ChessGame
castle pt g@(ChessGame {toPlay=ON c}) = if not $ elem pt [King, Queen] then same g else
    case castleKing pt c (board g) of
        Nothing -> same g
        Just b -> swap g{board=b}
castle _ g = same g



move :: Square -> Square -> ChessGame -> ChessGame
move _ _ g@(ChessGame{toPlay=OFF}) = same g
move start end g@(ChessGame{board=b, toPlay=ON gc}) = let p = lookupB start b in
    if not (isPiece p) || (gc /= pieceColor (justPiece p)) then same g else
        case makeMove (Move (justPiece p) start end) b of
            Nothing -> same g
            Just movedBoard -> updateGameState g{board=movedBoard}

updateGameState :: ChessGame -> ChessGame
updateGameState g = let new = evaluateGameState g in
    case gameState new of
        CheckMate _ -> off new
        Stalemate -> off new
        Active -> swap new
        _ -> swap new

move' :: Move -> ChessGame -> ChessGame
move' (Move _ a b) = move a b


--If your opponent is a computer, automatically get response
{- respond :: ChessGame -> ChessGame
respond g@ChessGame{board=b, toPlay= ON c} =
    case getRandomMove b c of
        Just rMove -> move' rMove g
        Nothing -> g{gameState=Failed}
respond g = g{gameState=Failed} -}

respond :: ChessGame -> ChessGame
respond g@ChessGame{board=b, toPlay= ON c} =
    case findBestMove b c of
        Just bMove -> move' bMove g
        Nothing -> g{gameState=Failed}
respond g = g{gameState=Failed}

evaluateGameState :: ChessGame -> ChessGame
evaluateGameState g@(ChessGame{board=b, gameState=gs}) =
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


isOver :: ChessGame -> Bool
isOver g = gameState g /= Active