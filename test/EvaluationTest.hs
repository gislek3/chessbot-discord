module EvaluationTest (module EvaluationTest) where

import Test.HUnit
import TestHelpers
import Chess.Board
import Chess.Piece
import Computer.Evaluation
import Data.Maybe (isJust, fromJust, isNothing)
import qualified Data.Map as M
import Computer.MoveFinder (findBestMove)


b :: Board
b = startingBoard

testBasicEvaluation :: Test
testBasicEvaluation = TestCase $ do
  assertBool "Starting position is evaluated to be equal" (evaluate b==0)
  let e2_e4 = makeMove (Move (Piece Pawn White False) (4,1) (4,3)) b
  assertBool "e4 is a valid move" (isJust e2_e4)
  let e7_e5 = makeMove (Move (Piece Pawn Black False) (4,6) (4,4)) $ fromJust e2_e4
  assertBool "e5 is a valid move" (isJust e7_e5)
  assertNotEqual "White has a slight advantage after opening move" (evaluate $ fromJust e2_e4) (0)
  assertEqual "Black is even after responding e5" (evaluate (fromJust e7_e5)) 0
  let removed_knight = clear (1,0) $ fromJust e2_e4
  assertEqual "Black has a huge advantage without white's knight" (evaluate removed_knight) (69)
  --assertBool "Black has a huge advantage without white's knight" (evaluate removed_knight < -300)



tests :: Test
tests = TestList [
    TestLabel "testBasicEvaluation" testBasicEvaluation
    ]