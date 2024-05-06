module Computer.Evaluation (evaluate, debugEvaluation) where

import Chess.Board
import Chess.Piece
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)


-- Main evaluation function using Reader monad
evaluate :: Reader Board Int
evaluate = do
    material <- evaluateMaterial
    threats <- calculateTotalThreats
    squares <- evaluateSquares
    return $ sum [material, threats, squares]

-- Debug evaluation using Reader monad
debugEvaluation :: Board -> M.Map String Int
debugEvaluation b = M.fromList [
    ("Activity score", runReader evaluateSquares b),
    ("Material score", runReader evaluateMaterial b),
    ("King threat score", runReader calculateTotalThreats b),
    ("King exposure score", runReader evaluateKingSafety b - runReader calculateTotalThreats b),
    ("Total score", runReader evaluate b)
    ]



-- Square value function using Reader monad
squareValue :: Square -> Reader Board Int
squareValue s = asks $ \_ -> fromMaybe 0 (M.lookup s squareValueMap)
  where
    squareValueMap = M.fromList [((x, y), calculateValue x y) | x <- [0..7], y <- [0..7]]
    centers = [(3, 3), (3, 4), (4, 3), (4, 4)]
    baseValue = 8
    calculateValue x y = max 0 $ baseValue - minimum (map (distanceFromCenter x y) centers)
    distanceFromCenter x0 y0 center = abs (x0 - fst center) + abs (y0 - snd center)

-- Evaluate squares using Reader monad
evaluateSquares :: Reader Board Int
evaluateSquares = do
    moves <- asks getAllMoves  -- Fetch all moves directly within the Reader monad
    mapM go (S.toList moves) >>= return . sum
  where
    go m = do
        sqValue <- squareValue (new_square m)
        let colorFactor = if pieceColor (piece m) == White then 1 else -1
        return $ sqValue * colorFactor



-- Evaluate material using Reader monad
evaluateMaterial :: Reader Board Int
evaluateMaterial = do
    b <- ask
    let pieces = getAllPieces b
    return $ sum $ map pieceValue pieces where
      pieceValue (Piece pieceType c _) = let m = if c==White then 1 else (-1) in case pieceType of
          Pawn   -> 150*m
          Knight -> 400*m
          Bishop -> 500*m
          Rook   -> 750*m
          Queen  -> 1000*m
          King   -> 0*m


calculateTotalThreats :: Reader Board Int
calculateTotalThreats = do
    b <- ask
    let whiteKingSqs = getSurroundings (getKingSquare White b)
    let blackKingSqs = getSurroundings (getKingSquare Black b)
    whiteMoves <- asks (S.toList . getAllColorMoves White)
    blackMoves <- asks (S.toList . getAllColorMoves Black)
    whiteThreats <- mapM (\move -> sum <$> mapM (isThreatTo move) whiteKingSqs) blackMoves
    blackThreats <- mapM (\move -> sum <$> mapM (isThreatTo move) blackKingSqs) whiteMoves
    return $ sum whiteThreats - sum blackThreats


evaluateKingSafety :: Reader Board Int
evaluateKingSafety = do
    threats <- calculateTotalThreats
    b <- ask
    let blackSurroundings = getSurroundings (getKingSquare Black b)
    let whiteSurroundings = getSurroundings (getKingSquare White b)
    blackExposed <- sum <$> mapM evaluateExposure blackSurroundings
    whiteExposed <- sum <$> mapM evaluateExposure whiteSurroundings
    return (threats + blackExposed - whiteExposed)


evaluateExposure :: Square -> Reader Board Int
evaluateExposure s = do
    b <- ask 
    return $ if isEmpty (lookupB s b) then 3 else 0

isThreatTo :: Move -> Square -> Reader Board Int
isThreatTo m s = asks $ \_ -> if new_square m == s then 6 else 0