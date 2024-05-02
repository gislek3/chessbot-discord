module COMPUTER.MoveFinder (getBestMove) where

import Chess.Board
import Chess.Piece
import Chess.Game
import Computer.Evaluation
import Data.Maybe (fromJust)
import Data.List (maximumBy, minimumBy)
import Data.Function (on)
import qualified Data.Set as S


-- Recursive minimax function with alpha-beta pruning
minimax :: Board -> Color -> Int -> Int -> Int -> Bool -> (Int, Maybe Move)
minimax b color depth alpha beta maximizingPlayer =
    if depth == 0 || isTerminal
        then (evaluate b, Nothing)
        else go moves alpha beta (if maximizingPlayer then -9999 else 9999, Nothing)
  where
    isTerminal = isOver game
    game = ChessGame { board = b, gameState = evaluateGameState initialGameState, updated = False }
    initialGameState = ChessGame { board = b, toPlay = ON color, playerColor = color, gameState = Active, updated = False }

    moves = S.toList $ getAllColorMoves color b

    go :: [Move] -> Int -> Int -> (Int, Maybe Move) -> (Int, Maybe Move)
    go [] _ _ best = best
    go (m:ms) a b (bestVal, bestMove) =
        case makeMove m b of
            Nothing -> go ms a b (bestVal, bestMove)
            Just newBoard ->
                let newColor = oppositeColor color
                    (score, _) = minimax newBoard newColor (depth - 1) a b (not maximizingPlayer)
                in if maximizingPlayer
                    then
                        if score > bestVal
                            then go ms (max a score) b (score, Just m)
                            else go ms a b (bestVal, bestMove)
                    else
                        if score < bestVal
                            then go ms a (min b score) (score, Just m)
                            else go ms a b (bestVal, bestMove)

-- Public function to get the best move for the current player
getBestMove :: ChessGame -> Int -> Maybe Move
getBestMove g@(ChessGame { board = b, toPlay = ON color }) depth =
    snd $ minimax b color depth (-9999) 9999 True
getBestMove _ _ = Nothing


getRandomMove :: ChessGame -> Maybe Move
getRandomMove g@(ChessGame{toPlay=tp, board=b}) = let possible = getAllLegalColorMoves tp b
    in if null possible then Nothing else S.elemAt 0 possible