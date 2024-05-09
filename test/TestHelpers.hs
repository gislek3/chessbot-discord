module TestHelpers (module TestHelpers) where

import Text.Megaparsec (ParseErrorBundle)
import Data.Text (Text)
import Data.Void (Void)
import Parsing.ChessParser
import Chess.Board (Square, Move(..), Board, lookupB, makeMove, isValidSquare)
import Chess.Game
import Test.HUnit

isParseError :: Either (ParseErrorBundle Text Void) ChessCommand -> Bool
isParseError (Left _) = True
isParseError _ = False

assertNotEqual :: (Eq a) => String -> a -> a -> Assertion
assertNotEqual message expected actual =
  assertBool message (expected /= actual)

mHelper :: (Square, Square) -> Board -> Move
mHelper (start, stop) b = do
  case lookupB start b of
    Occupied p -> if (isValidSquare start && isValidSquare stop)
      then Move p start stop else error "Move is illegal"
    _ -> error "Move is illegal"

mMap :: [(Square, Square)] -> Board -> Board
mMap [] b = b
mMap (x:xs) b = do
  let newMove = mHelper x b
  let newBoard = makeMove newMove b
  case newBoard of
    Just valid -> mMap xs valid
    Nothing -> b


