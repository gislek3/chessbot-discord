module Computer.MoveFinder (getBestMove, getRandomMove) where

import Chess.Board
import Chess.Piece
import Computer.Evaluation
import Data.Maybe (fromJust)
import Data.List (maximumBy, minimumBy)
import Data.Function (on)
import qualified Data.Set as S
import Chess.Board (gameIsOver)
import Data.List (sortBy)

-- Recursive minimax function with alpha-beta pruning
minimax :: Board -> Color -> Int -> (Int, Maybe Move)
minimax board color depth =
    if depth == 0 || gameIsOver board
        then (evaluate board, Nothing)
        else chooseBestMove (getTopMoves board color)
  where
    chooseBestMove :: [(Int, Move)] -> (Int, Maybe Move)
    chooseBestMove [] = (0, Nothing)
    chooseBestMove ((score, move):rest) =
        let (nextScore, _) = minimax (fromJust $ makeMove move board) (oppositeColor color) (depth - 1)
            newScore = score - nextScore
        in if null rest || newScore > bestScore
            then (newScore, Just move)
            else chooseBestMove rest
        where
            bestScore = if null rest then 0 else fst $ head rest

    getTopMoves :: Board -> Color -> [(Int, Move)]
    getTopMoves b c = take depth $ sortBy (compare `on` fst) $ map (\m -> (evaluateBoardAfterMove m, m)) $ S.toList $ getAllLegalColorMoves c b

    evaluateBoardAfterMove :: Move -> Int
    evaluateBoardAfterMove move =
        let newBoard = fromJust $ makeMove move board
        in evaluate newBoard

-- Public function to get the best move for the current player
getBestMove :: Board -> Color -> Int -> Maybe Move
getBestMove board color depth =
    snd $ minimax board color depth

-- Function to get a random move from the list of legal moves
getRandomMove :: Board -> Color -> Maybe Move
getRandomMove board color = 
    let moves = S.toList $ getAllLegalColorMoves color board
    in if null moves then Nothing else Just $ head moves
