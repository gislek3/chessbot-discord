module Computer.MoveFinder (findBestMove) where

import qualified Data.Set as S
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Chess.Board
import Chess.Piece
import Computer.Evaluation

-- Assuming the rest of the Chess.Board module is as defined earlier

-- This function evaluates the best move for a given color on the board
import qualified Data.Set as S
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.List (maximumBy)

-- Assuming the rest of the Chess.Board module is as defined earlier

-- This function evaluates the best move for a given color on the board
findBestMove :: Board -> Color -> Maybe Move
findBestMove board color = 
    let moves = S.toList $ getAllColorMoves color board
        scoredMoves = [(move, scoreMove move board) | move <- moves]
    in fst <$> safeMaximumBy (\(_, score1) (_, score2) -> compare score1 score2) scoredMoves

-- Score a move based on the resulting board state
scoreMove :: Move -> Board -> Int
scoreMove move board = 
    case makeMove move board of
        Nothing -> minBound
        Just newBoard -> 
            - bestOpponentResponse newBoard (oppositeColor $ pieceColor $ piece move) -- Minimize the opponent's best outcome

-- Compute the best response score for the opponent
bestOpponentResponse :: Board -> Color -> Int
bestOpponentResponse board color =
    let responses = S.toList $ getAllColorMoves color board
        opponentScores = map (\resp -> evaluateMove resp board) responses
    in if null opponentScores then 0 else maximum opponentScores

-- Evaluate a move simply by looking at the board state or a more sophisticated heuristic
evaluateMove :: Move -> Board -> Int
evaluateMove move board = 
    case makeMove move board of
        Just b -> evaluate b -- Assuming evaluateBoard function exists to evaluate the board
        Nothing -> minBound

-- Safe maximum by function to prevent errors on empty lists
safeMaximumBy :: Ord b => ((a, b) -> (a, b) -> Ordering) -> [(a, b)] -> Maybe (a, b)
safeMaximumBy _ [] = Nothing
safeMaximumBy cmp xs = Just $ maximumBy cmp xs