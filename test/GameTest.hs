module GameTest (tests) where

import Test.HUnit
import Chess.Game


g :: ChessGame
g = defaultStart

testMoveInGame :: Test
testMoveInGame= TestList
  [ TestCase $ do
      let e2_e4 = ((4,1),(4,3))
      let moved = move (fst e2_e4) (snd (e2_e4)) g
      assertBool "Moving a piece should succeed" (updated moved)
  ]

tests :: Test
tests = TestList [
    TestLabel "testMoveInGame" testMoveInGame
    ]