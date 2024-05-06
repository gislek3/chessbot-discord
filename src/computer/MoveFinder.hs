module Computer.MoveFinder (findBestMove) where

import qualified Data.Set as S
import Chess.Board
import Chess.Piece
import Computer.Evaluation
import Control.Monad.Reader
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.List (maximumBy)


--This function evaluates the best move for a given color on the board
findBestMove :: Board -> Color -> Maybe Move
findBestMove board color = 
    let moves = S.toList $ getAllColorMoves color board
        scoredMoves = [(move, scoreMove move board) | move <- moves]
    in fst <$> safeMaximumBy (\(_, score1) (_, score2) -> compare score1 score2) scoredMoves

--
scoreMove :: Move -> Board -> Int
scoreMove move board = 
    case makeMove move board of
        Nothing -> minBound
        Just newBoard -> 
            - bestOpponentResponse newBoard (oppositeColor $ pieceColor $ piece move) -- Minimize the opponent's best outcome

--Compute the best response score
bestOpponentResponse :: Board -> Color -> Int
bestOpponentResponse board color =
    let responses = S.toList $ getAllColorMoves color board
        opponentScores = map (\resp -> evaluateMove resp board) responses
    in if null opponentScores then 0 else maximum opponentScores


--Can be ran "for each", checks the score of a move, if legal, on the board
evaluateMove :: Move -> Board -> Int
evaluateMove move board = 
    case makeMove move board of
        Just b -> runReader evaluate b
        Nothing -> minBound

-- Safe maximum by function to prevent errors on empty lists
safeMaximumBy :: Ord b => ((a, b) -> (a, b) -> Ordering) -> [(a, b)] -> Maybe (a, b)
safeMaximumBy _ [] = Nothing
safeMaximumBy cmp xs = Just $ maximumBy cmp xs