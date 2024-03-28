module GameTest (tests) where

import Test.HUnit
import Chess.Game
import Chess.Piece
import Chess.Board


game :: ChessGame
game = defaultStart

testMoveInGame :: Test
testMoveInGame= TestList
  [ TestCase $ do
      let e2_e4 = ((4,1),(4,3))
      let moved = move (fst e2_e4) (snd (e2_e4)) game
      assertEqual "There is a pawn at e2" (lookupB (4,1) $ board game) (Occupied $ startP Pawn White)
      assertBool "Moving a piece should succeed" (updated moved),

      TestCase $ do
        let nf3 = ((6,0),(5,2))
        let moved = move (fst nf3) (snd nf3) game
        assertBool "Opening move Nf3 succeeds" (updated moved)
  ]

tests :: Test
tests = TestList [
    TestLabel "testMoveInGame" testMoveInGame
    ]