module Computer.Evaluation (evaluate) where

import Chess.Board
import Chess.Piece
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)


--Score the current board
evaluate :: Reader Board Int
evaluate = do
    material <- evaluateMaterial
    threats <- calculateTotalThreats
    squares <- evaluateSquares
    return $ sum [material, threats, squares]


--Score a square based on centrality, providing higher value to more "active squares"
squareValue :: Square -> Reader Board Int
squareValue s = asks $ \_ -> fromMaybe 0 (M.lookup s squareValueMap)
  where
    squareValueMap = M.fromList [((x, y), calculateValue x y) | x <- [0..7], y <- [0..7]]
    centers = [(3, 3), (3, 4), (4, 3), (4, 4)]
    baseValue = 8
    calculateValue x y = max 0 $ baseValue - minimum (map (distanceFromCenter x y) centers)
    distanceFromCenter x0 y0 center = abs (x0 - fst center) + abs (y0 - snd center)


--For each square, score it (could probably make this a lot less lines of code)
evaluateSquares :: Reader Board Int
evaluateSquares = do
    moves <- asks getAllMoves
    mapM go (S.toList moves) >>= return . sum
  where
    go m = do
        sqValue <- squareValue (new_square m)
        let colorFactor = if pieceColor (piece m) == White then 1 else -1
        return $ sqValue * colorFactor


--Evaluate material on the board
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


--Check how attacked the kingsides are
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


--Scoring function for each "threatened king square"
isThreatTo :: Move -> Square -> Reader Board Int
isThreatTo m s = asks $ \_ -> if new_square m == s then 6 else 0
