module Computer.Evaluation (evaluate, debugEvaluation) where

import Chess.Board
import Chess.Piece

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Chess.Board (getAllColorMoves)

type BoardValue = Int
type ValueMap = M.Map Square BoardValue


--TODO:  am not consdiering the AMOUNT of squares, total piece activity... or am I?????
squareValue :: Square -> Int
squareValue s = fromMaybe 0 (M.lookup s squareValueMap)
    where
        squareValueMap = M.fromList [((x, y), calculateValue x y) | x <- [0..7], y <- [0..7]]
        centers = [(3, 3), (3, 4), (4, 3), (4, 4)]  -- The central four squares: d4, e4, d5, e5
        baseValue = 8   -- Base value for centrality
        calculateValue x y = max 0 $ baseValue - minimum (map (distanceFromCenter x y) centers)
        distanceFromCenter x0 y0 center = abs (x0 - fst center) + abs (y0 - snd center)


evaluateSquares :: Board -> Int
evaluateSquares b = sum $ map go (S.toList $ getAllMoves b) where
    go m = squareValue (new_square m) * (if pieceColor (piece m)==White then 1 else -1)


evaluateMaterial :: Board -> Int
evaluateMaterial b = sum $ map pieceValue pieces where
    pieces = getAllPieces b
    pieceValue (Piece pieceType c _) = let m = if c==White then 1 else (-1) in case pieceType of
        Pawn   -> 100*m
        Knight -> 300*m
        Bishop -> 350*m
        Rook   -> 600*m
        Queen  -> 900*m
        King   -> 0*m


evaluateExposure :: Square -> Board -> Int
evaluateExposure s b = if isEmpty (lookupB s b) then 3 else 0


isThreatTo :: Move -> Square -> Int
isThreatTo m s = if new_square m==s then 6 else 0

calculateTotalThreats :: Board -> Int
calculateTotalThreats b = do
    let whiteKingSqs = getSurroundings (getKingSquare White b)
    let blackKingSqs = getSurroundings (getKingSquare Black b)
    let whiteMoves = S.toList $ getAllColorMoves White b
    let blackMoves = S.toList $ getAllColorMoves Black b
    let whiteThreats = [isThreatTo move square | move <- blackMoves, square <- whiteKingSqs]
    let blackThreats = [isThreatTo move square | move <- whiteMoves, square <- blackKingSqs]
    sum whiteThreats + (-1)*sum blackThreats



evaluateKingSafety :: Board -> Int
evaluateKingSafety b = do
    let blackExposed = (-1) * (sum $ map (\s -> evaluateExposure s b) (getSurroundings (getKingSquare Black b)))
    let whiteExposed = sum $ map (\s -> evaluateExposure s b) (getSurroundings (getKingSquare White b))
    calculateTotalThreats b + blackExposed + whiteExposed


debugEvaluation :: Board -> M.Map String Int
debugEvaluation b = M.fromList [
        ("Activity score", evaluateSquares b),
        ("Material score", evaluateMaterial b),
        ("King threat score", calculateTotalThreats b),
        ("King exposure score", evaluateKingSafety b-(calculateTotalThreats b)),
        ("Total score", evaluate b)
    ]

--TODO: replace with Reader-monad to save less parameters bro lets gooooo!!!!
evaluate :: Board -> Int
evaluate b = sum [evaluateMaterial b, evaluateSquares b]