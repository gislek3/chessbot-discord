module GameTest (tests) where

import Test.HUnit
import Chess.Game
import Chess.Piece
import Chess.Board
import TestHelpers


{-
WARNING: deprecated

These tests were made with an older version of ChessGame. It was based on a version
that was not hard-coded to recieve responses to moves by the user. It can be re-introduced
by allowing the ChessGame.hs module to faiciliate situations where both players are controlled
by humans, or introducing some sort of debug flag.

At the time of intentional deprecation, all tests passed.
-}


game :: ChessData
game = defaultStart

applyMoves :: ChessData -> [(Square, Square)] -> ChessData
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
  TestCase $ do -- Check that the game doesn't update when it shouldn't
    let nf3 = ((6,0),(5,2))
    let e5 = ((4,6),(4,4))
    assertBool "Black can't move first" (not $ updated $ uncurry move e5 game)
    let after_nf3 = uncurry move nf3 game
    assertBool "Opening move Nf3 works" (updated after_nf3)
    let nh3 = ((5,2),(7,3))
    assertBool "Knight can't move again when it's black's turn" (not $ updated $ uncurry move nh3 after_nf3)
    assertBool "Black can move after the turns have swapped again" (updated $ uncurry move e5 after_nf3)
  
  , TestCase $ do -- Verify that the toPlay state is consistent
    assertEqual "Game state is set to active" (gameState game) Active
    assertEqual "Whites turn #1" (toPlay game) (ON White)
    let e2_e4 = move (4,1) (4,3) game
    assertEqual "Black's turn #1" (toPlay e2_e4) (ON Black)
    let e7_e5 = move (4,6) (4,4) e2_e4
    assertEqual "White's turn #2" (toPlay e7_e5) (ON White)
    let nf3 = move (6,0) (5,2) e7_e5
    assertEqual "Black's turn #2" (toPlay nf3) (ON Black)
  ]

testCannotMoveAfterGameOver :: Test
testCannotMoveAfterGameOver = TestList [
    TestCase $ do
      let ended_game = resign game
      let nf3 = ((6,0),(5,2))
      let moved = uncurry move nf3 ended_game
      assertBool "Opening move Nf3 fails after resignation" (not $ updated moved)
  ]

testCheck :: Test
testCheck = TestList [
    TestCase $ do -- Can be placed into check
      let moveList = [((4,1),(4,3)), ((5,6),(5,4)), ((3,0),(7,4))]
      let checksBlack = mMapGame moveList game
      assertEqual "Queen has moved to h5" (Occupied $ Piece Queen White True) (lookupB (7,4) $ board checksBlack)
      assertBool "The game state reflects that the king is in check" (gameState checksBlack == InCheck Black)
      let whiteMoves = getAllColorMoves White $ board checksBlack
      assertBool "The assertion function matches the game state" $ kingIsInCheck Black whiteMoves (board checksBlack)
    
    , TestCase $ do -- You can't place yourself into check
      let moveList = [((4,1),(4,3)), ((0,6),(0,5)), ((3,0),(7,4))]
      let readyToCheck = mMapGame moveList game
      assertEqual "Queen has moved to h5" (Occupied $ Piece Queen White True) (lookupB (7,4) $ board readyToCheck)
      assertBool "Black king is not in check initially" (gameState readyToCheck /= InCheck Black)
      let attempt = mMapGame [((5,6),(5,4))] readyToCheck
      assertEqual "Board has not altered after attempt at illegal move" (board attempt) (board readyToCheck)

    
    , TestCase $ do --You can get yourself out of check
      let moveList = [((4,1),(4,3)), ((5,6),(5,4)), ((3,0),(7,4))]
      let checksBlack = mMapGame moveList game
      assertEqual "Queen has moved to h5" (Occupied $ Piece Queen White True) (lookupB (7,4) $ board checksBlack)
      assertBool "Black is in check" (gameState checksBlack == InCheck Black)
      assertEqual "Black's turn to play after getting put in check" (toPlay checksBlack) (ON Black)
      
      let badMove = move (1,7) (2,5) checksBlack
      assertEqual "Black's turn to play still after badmove" (toPlay badMove) (ON Black)
      assertBool "Game is not updated after move that doesn't put you out of check" (not $ updated badMove)
      
      let goodMove = move (6,6) (6,5) checksBlack
      assertBool "Game is updated after move that gets you out of check" (updated goodMove) 
      assertEqual "White's turn to play again after black is out of check" (toPlay goodMove) (ON White)
      assertEqual "Game is now active again, and nobody is in check" (gameState goodMove) (Active)
  ]

--If you are in check and cannot move out of check, then the game should be able to verify this
testCheckmate :: Test
testCheckmate = TestList [
    TestCase $ do --Fool's mate: e4,g5,d4,f5,Qh5
      let moveList = [((4,1),(4,3)), ((6,6),(6,4)), ((3,1),(3,3)), ((5,6),(5,4)), ((3,0),(7,4))]
      let fool = applyMoves game moveList
      assertBool "Fool's mate puts black in check mate." (gameState fool == CheckMate Black)
  ]

testStalemate :: Test
testStalemate = TestList [
    TestCase $ do 
      let wq = Piece Queen White True
      let wk = Piece King White True
      let bk = Piece King Black True
      let staleBoard = place (0,0) wk (place (2,0) wq (place (0,7) bk empty))
      let staleGame = game{board=staleBoard}
      let stalemate = move (2,0) (2,6) staleGame

      assertEqual "Game is stalemated" (gameState stalemate) (Stalemate)
      assertEqual "Game is over" (toPlay stalemate) (OFF)
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