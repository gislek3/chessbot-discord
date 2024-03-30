module GameTest (tests) where

import Test.HUnit
import Chess.Game
import Chess.Piece
import Chess.Board


game :: ChessGame
game = defaultStart

applyMoves :: ChessGame -> [(Square, Square)] -> ChessGame
applyMoves = foldl (\g (start, end) -> move start end g)

testMoveInGame :: Test
testMoveInGame= TestList
  [ TestCase $ do
      let e2_e4 = ((4,1),(4,3))
      let moved = uncurry move e2_e4 game
      assertEqual "There is a pawn at e2" (lookupB (4,1) $ board game) (Occupied $ startP Pawn White)
      assertBool "Moving a piece should succeed" (updated moved),

      TestCase $ do
        let nf3 = ((6,0),(5,2))
        let moved = uncurry move nf3 game
        assertBool "Opening move Nf3 succeeds" (updated moved)
  ]

testTurnSwitching :: Test
testTurnSwitching = TestList [
  TestCase $ do
    let nf3 = ((6,0),(5,2))
    let e5 = ((4,6),(4,4))
    assertBool "Black can't move first" (not $ updated $ uncurry move e5 game)
    let after_nf3 = uncurry move nf3 game
    assertBool "Opening move Nf3 works" (updated after_nf3)
    let nh3 = ((5,2),(7,3))
    assertBool "Knight can't move again when it's black's turn" (not $ updated $ uncurry move nh3 after_nf3)
    assertBool "Black can move after the turns have swapped again" (updated $ uncurry move e5 after_nf3)
  ]

testCannotMoveAfterGameOver :: Test
testCannotMoveAfterGameOver = TestList [
    TestCase $ do
      let ended_game = resign game
      let nf3 = ((6,0),(5,2))
      let moved = uncurry move nf3 ended_game
      assertBool "Opening move Nf3 fails after resignation" (not $ updated moved)
  ]

--Fool's mate: e4,g5,d4,f5,Qh5
testCheckmate :: Test
testCheckmate = TestList [
    TestCase $ do
      --Fool's mate
      let moveList = [((4,1),(4,3)), ((6,6),(6,4)), ((3,1),(3,3)), ((5,6),(5,4)), ((3,0),(7,4))]
      let fool = applyMoves game moveList
      assertBool "Fool's mate puts black in check mate." (gameState fool == CheckMate Black)

  ]

tests :: Test
tests = TestList [
    TestLabel "testMoveInGame" testMoveInGame,
    TestLabel "testCannotMoveAfterGameOver" testCannotMoveAfterGameOver,
    TestLabel "testTurnSwitching" testTurnSwitching,
    TestLabel "testCheckmate" testCheckmate
    ]