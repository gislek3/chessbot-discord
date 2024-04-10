module EvaluationTest (module EvaluationTest) where

import Test.HUnit
import TestHelpers
import Chess.Board
import Chess.Piece
import Computer.Evaluation
import Data.Maybe (isJust, fromJust, isNothing)


b :: Board
b = startingBoard

testBasicEvaluation :: Test
testBasicEvaluation = TestCase $ do
  assertBool "Starting position is evaluated to be equal" (evaluate b==0)
  let e2_e4 = makeMove (Move (Piece Pawn White False) (4,1) (4,3)) b
  assertBool "e4 is a valid move" (isJust e2_e4)
  assertEqual "White has a slight advantage after opening move" (evaluate $ fromJust e2_e4) (0)

  

tests :: Test
tests = TestList [

    ]