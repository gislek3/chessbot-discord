module GameTest (tests) where

import Test.HUnit
import Chess.Game
import Chess.Piece
import Chess.Board
import Computer.MoveFinder
import TestHelpers
import Data.Maybe (isNothing)

game :: ChessGame
game = defaultStart


testMoveInGame :: Test
testMoveInGame= TestList
  [ 
    TestCase $ do
      assertEqual "There is a pawn at e2" (lookupB (4,1) $ board game) (Occupied $ startP Pawn White)
      let e2_e4 = ((4,1),(4,3))
      let moved = uncurry move e2_e4 game
      assertEqual "Pawn is at e4" (lookupB (4,3) $ board moved) (Occupied $ Piece Pawn White True)
      assertBool "Moving a piece should succeed" (updated moved)
      assertEqual "debug" Chess.Board.empty (board moved)
      assertEqual "this passes" (gameState moved) (Active)
  ]

testTurnSwitching :: Test
testTurnSwitching = TestList [
  ]

testCannotMoveAfterGameOver :: Test
testCannotMoveAfterGameOver = TestList [
  ]

testCheck :: Test
testCheck = TestList [
  ]

--If you are in check and cannot move out of check, then the game should be able to verify this
testCheckmate :: Test
testCheckmate = TestList [
  ]

testStalemate :: Test
testStalemate = TestList [
  ]

tests :: Test
tests = TestList [
    TestLabel "testMoveInGame" testMoveInGame,
    TestLabel "testCannotMoveAfterGameOver" testCannotMoveAfterGameOver,
    TestLabel "testTurnSwitching" testTurnSwitching,
    TestLabel "testCheckmate" testCheckmate,
    TestLabel "testCheck" testCheck,
    TestLabel "testStalemate" testStalemate
    ]