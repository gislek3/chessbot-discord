module Computer.MoveFinder (getBestMove, getRandomMove) where

import Chess.Board
import Chess.Piece
import Computer.Evaluation
import Data.Maybe (fromJust)
import Data.List (maximumBy, minimumBy)
import Data.Function (on)
import qualified Data.Set as S
import Chess.Board (gameIsOver)

-- Recursive minimax function with alpha-beta pruning
minimax :: Board -> Color -> Int -> Int -> Int -> Bool -> (Int, Maybe Move)
minimax board color depth alpha beta maximizingPlayer =
    if depth == 0 || isTerminal
        then (evaluate board, Nothing)
        else go moves alpha beta (if maximizingPlayer then -9999 else 9999, Nothing)
  where
    isTerminal = gameIsOver board  -- Assuming you have a function to check game over conditions
    moves = S.toList $ getAllLegalColorMoves color board

    go :: [Move] -> Int -> Int -> (Int, Maybe Move) -> (Int, Maybe Move)
    go [] _ _ best = best
    go (m:ms) a b (bestVal, bestMove) =
        case makeMove m board of
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
getBestMove :: Board -> Color -> Int -> Maybe Move
getBestMove board color depth =
    snd $ minimax board color depth (-9999) 9999 True

-- Function to get a random move from the list of legal moves
getRandomMove :: Board -> Color -> Maybe Move
getRandomMove board color = 
    let moves = S.toList $ getAllLegalColorMoves color board
    in if null moves then Nothing else Just $ head moves
