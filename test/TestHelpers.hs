module TestHelpers (module TestHelpers) where

import Text.Megaparsec (ParseErrorBundle)
import Data.Text (Text)
import Data.Void (Void)
import Parsing.ChessParser (ChessCommand(..))
import Chess.Board (Square, Move(..), Board, SquareContent(..), lookupB, makeMove, isValidSquare)
import Chess.Game
import Test.HUnit

isParseError :: Either (ParseErrorBundle Text Void) ChessCommand -> Bool
isParseError (Left _) = True
isParseError _ = False

assertNotEqual :: (Eq a) => String -> a -> a -> Assertion
assertNotEqual message expected actual =
  assertBool message (expected /= actual)

mHelper :: (Square, Square) -> Board -> Move
mHelper (start, stop) board = do
  case lookupB start board of
    Occupied p -> if (isValidSquare start && isValidSquare stop)
      then Move p start stop else error "Move is illegal"
    _ -> error "Move is illegal"

mMap :: [(Square, Square)] -> Board -> Board
mMap [] board = board
mMap (x:xs) board = do
  let newMove = mHelper x board
  let newBoard = makeMove newMove board
  case newBoard of
    Just valid -> mMap xs valid
    Nothing -> board

mMap' :: [(Square, Square)] -> ChessGame -> ChessGame
mMap' l g@(ChessGame{board=b}) = evaluateGameState $ g{board=mMap l b}



mMapGame :: [(Square, Square)] -> ChessGame -> ChessGame
mMapGame [] game = game
mMapGame (x:xs) game = do
  let moved = uncurry move x game
  if (not $ updated moved) then game
  else mMapGame xs moved
