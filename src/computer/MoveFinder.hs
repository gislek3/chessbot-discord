module Computer.MoveFinder (findBestMove) where

--Imports
import qualified Data.Set as S
import Chess.Board
import Chess.Piece
import Computer.Evaluation
import Data.Maybe()
import Data.Ord()
import Data.List (maximumBy)


{-
Finds the best possible move based on the Evaluation modaule. It takes a minMax approach
to try to find the move that gives the best score considering the best available response.
-}


--TODO: Remove/semi-arbitrate determinism
--TODO: Increase depth, perhaps dynamically? At least to one more step to prevent one-twos like forks.
-- | This function evaluates the best move for a given color on the board
findBestMove :: Board -> Color -> Maybe Move
findBestMove board color =
    let moves = S.toList $ getAllLegalColorMoves color board
        scoredMoves = [(move, scoreMove move board) | move <- moves]
    in fst <$> safeMaximumBy (\(_, score1) (_, score2) -> compare score1 score2) scoredMoves

-- | Score a move "in context", i.e. taking the opponents response into consideration
scoreMove :: Move -> Board -> Int
scoreMove move board =
    case makeMove move board of
        Nothing -> minBound
        Just newBoard ->
            - bestOpponentResponse newBoard (oppositeColor $ pieceColor $ piece move) -- Minimize the opponent's best outcome
            where
                --Score the board after the move has been applied
                evaluateMove m b = maybe minBound evaluate (makeMove m b)
                --Maximizing opponent response
                bestOpponentResponse b color =
                    let responses = S.toList $ getAllLegalColorMoves color b
                        opponentScores = map (`evaluateMove` b) responses
                    in if null opponentScores then 0 else maximum opponentScores


-- | Safe maximum by function to prevent errors on empty lists
safeMaximumBy :: ((a, b) -> (a, b) -> Ordering) -> [(a, b)] -> Maybe (a, b)
safeMaximumBy _ [] = Nothing
safeMaximumBy cmp xs = Just $ maximumBy cmp xs
