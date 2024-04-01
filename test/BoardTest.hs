module BoardTest (tests) where

import Test.HUnit
import TestHelpers
import Chess.Board
import Chess.Piece
import Data.Maybe (isJust, fromJust, isNothing)
import qualified Data.Set as S
import qualified Data.Map as M

{-
--TODO: create cantPutYourselfInCheckTest
-}
b :: Board
b = startingBoard


testLookup :: Test
testLookup = TestList
  [ TestCase $ assertEqual "Can find piece #1" (Occupied (Piece Rook White False)) (lookupB (0,0) b),
    TestCase $ assertEqual "Can find piece #2" (Occupied (Piece Rook Black False)) (lookupB (7,7) b),
    TestCase $ assertEqual "Can find piece #3" (Occupied (Piece Pawn Black False)) (lookupB (1,6) b),
    TestCase $ assertEqual "Can find piece #4 " (Occupied (Piece King Black False)) (lookupB (4,7) b),
    
    TestCase $ assertEqual "Finds nothing #1" (Empty) (lookupB (4,4) b),
    TestCase $ assertEqual "Finds nothing #2" (Empty) (lookupB (4,5) b),
    TestCase $ assertEqual "Finds nothing #3" (Empty) (lookupB (2,3) b),

    TestCase $ assertEqual "Illegal square #1" (Illegal) (lookupB (9,4) b),
    TestCase $ assertEqual "Illegal square #2" (Illegal) (lookupB (4,-3) b),
    TestCase $ assertEqual "Illegal square #3" (Illegal) (lookupB (8,0) b)
  ]

testClear :: Test
testClear = TestCase $ do
  let newBoard = clear (0,0) b --remove the white rook in the left-hand corner
  assertNotEqual "Check that they are not equal" b newBoard
  assertEqual "Removing empty squares does nothing" newBoard $ clear (0,0) b
  assertEqual "Removing illegal squares does nothing" newBoard $ clear (9,9) newBoard
  assertEqual "A board with a piece removed can be restored" b (place (0,0) (Piece Rook White False) newBoard)


testPlace :: Test
testPlace = TestCase $ do
  let emptyBoard = empty
  let boardWithBlackRook = place (0,0) (Piece Rook Black False) emptyBoard
  assertNotEqual "Placing a piece should change the board" emptyBoard boardWithBlackRook
  assertEqual "Placing the same piece in the same position should yield the same board" boardWithBlackRook (place (0,0) (Piece Rook Black False) boardWithBlackRook)
  let boardWithTwoPieces = place (1,1) (Piece Knight White False) boardWithBlackRook
  assertNotEqual "Placing a different piece in a new position should change the board" boardWithBlackRook boardWithTwoPieces
  let boardAfterIllegalPlace = place (9,9) (Piece Pawn Black False) boardWithBlackRook
  assertEqual "Placing a piece at an illegal position should not change the board" boardWithBlackRook boardAfterIllegalPlace
  let boardAfterRemoval = clear (0,0) boardWithBlackRook
  assertEqual "Removing the placed piece should restore the original board" emptyBoard boardAfterRemoval


testMove :: Test
testMove = TestList
  [ TestCase $ do
      let emptyBoard = empty
      let startBoard = place (0,0) (Piece Rook Black False) emptyBoard
      let moveResult = makeMove (Move (Piece Rook Black False) (0,0) (0,1)) startBoard
      assertBool "Moving a piece should succeed" (isJust moveResult)

      , TestCase $ do
      let emptyBoard = empty
      let startBoard = place (0,0) (Piece Rook Black False) emptyBoard
      let moveResult = makeMove (Move (Piece Rook Black False) (0,0) (0,-2)) startBoard
      assertBool "Moving a piece to an illegal position should fail" (isNothing moveResult)
      
      , TestCase $ do
      let emptyBoard = empty
      let startBoard = place (0,0) (Piece Rook Black False) . place (0,1) (Piece Knight White False) $ emptyBoard
      let moveResult = makeMove (Move (Piece Rook Black False) (0,0) (0,1)) startBoard
      assertBool "Moving a piece to a square occupied by an enemy piece should succeed" (isJust moveResult)

      , TestCase $ do
      let emptyBoard = empty
      let startBoard = place (6,0) (startP Knight White) . place (5,1) (startP Pawn White) . place (6,1) (startP Pawn White) $ emptyBoard
      let moveResult = makeMove (Move (Piece Knight White False) (6,0) (5,2)) startBoard
      assertBool "Opening move: Nf3" (isJust moveResult)

      , TestCase $ do
      let startBoard = startingBoard
      let moveResult = makeMove (Move (Piece Pawn White False) (4,1) (4,3)) startBoard
      assertBool "King's pawn opening succeeds" (isJust moveResult)

      , TestCase $ do
      let emptyBoard = empty
      let startBoard = place (0,0) (Piece Rook Black False) . place (0,1) (Piece Pawn Black False) $ emptyBoard
      let moveResult = makeMove (Move (Piece Rook Black False) (0,0) (0,1)) startBoard
      assertBool "Moving a piece to a square occupied by a friendly piece should fail" (isNothing moveResult)

  ]

--TODO: test move function for different pieces
--TODO: test move function that specifically checks that you don't put your own king in check, includes directly and indirectly


testPromotion :: Test
testPromotion = TestList [
  TestCase $ do
    let emptyBoard = empty
    let startBoard = place (0,6) (Piece Pawn White True) emptyBoard
    let moveResult = makeMove (Move (Piece Pawn White True) (0,6) (0,7)) startBoard
    assertBool "Move succeeds" (isJust moveResult)
    assertEqual "Pawn has been promoted." (Occupied (Piece Pawn White True)) (lookupB (0,7) $ fromJust moveResult)
  ]

--TODO: integrate black pawns
testGetMovesPawn :: Test
testGetMovesPawn = TestList [
  TestCase $ do --Normal forward moves work
    let e2pos = (4, 1)
    let e2piece = fromJust $ M.lookup e2pos b
    let possibleMoves = getMoves (e2piece, e2pos) b
    let expectedMoves = S.fromList [Move e2piece (4,1) (4,2), Move e2piece (4,1) (4,3)]
    assertEqual "Case #1: Pawn at e2 can move to e3 and e4" expectedMoves possibleMoves,
  TestCase $ do --Attacking diagonals work, and they don't override normal forward moves.
    let e2pos = (4, 1)
    let attackablesAdded = place (5,2) (startP Rook Black) (place (3,2) (startP Bishop Black) (place (2,3) (startP Pawn Black) b))
    let e2piece = fromJust $ M.lookup e2pos attackablesAdded
    let possibleMoves = getMoves (e2piece, e2pos) attackablesAdded
    let expectedMoves = S.fromList [Move e2piece (4,1) (4,2), Move e2piece (4,1) (4,3), Move e2piece (4,1) (5,2), Move e2piece (4,1) (3,2)]
    assertEqual "Case #2: Pawn at e2 can also attack its diagonals" expectedMoves possibleMoves,
  TestCase $ do --Double move is only valid once
    let e2pos = (4, 1)
    let e2piece = fromJust $ M.lookup e2pos b
    let movedOnce = makeMove (Move e2piece (4,1) (4,2)) b
    assertBool "Case #3a: Initial move to e3 is valid" (isJust movedOnce)
  ]

testCastle :: Test
testCastle = TestList [
    TestCase $ do --Kingside castle as white
      let startBoard = place (4,0) (Piece King White False) (place (7,0) (Piece Rook White False) empty)
      assertBool "Kingside castle with no obstacles" (isJust (castle King White startBoard))
      let inCheck = place (4,4) (Piece Rook Black True) startBoard
      let unSafe = place (4,4) (Piece Rook Black True) startBoard
      let moved = fromJust $ makeMove (Move (Piece King White False) (5,0) (4,0)) $ fromJust (makeMove (Move (Piece King White True) (4,0) (5,0)) startBoard)
      assertBool "Kingside castle while in check" (isNothing (castle King White inCheck))
      assertBool "Kingside castle onto unsafe squares" (isNothing (castle King White unSafe))
      assertBool "Kingside castle with moved pieces" (isNothing (castle King White moved))
      let afterCastle = fromJust $ castle King White startBoard
      assertEqual "King has moved after castling" (lookupB (6,0) afterCastle) (Occupied$Piece King White True)
      assertEqual "Rook has moved after castling" (lookupB (5,0) afterCastle) (Occupied$Piece Rook White True)

    , TestCase $ do --Queenside castle as black
      let startBoard = place (4,7) (Piece King Black False) (place (0,7) (Piece Rook Black False) empty)
      assertBool "Queenside castle with no obstacles" (isJust (castle King Black startBoard))
      let inCheck = place (4,4) (Piece Rook White True) startBoard
      let unSafe = place (3,3) (Piece Rook White True) startBoard
      let moved = fromJust $ makeMove (Move (Piece King Black False) (3,7) (4,7)) $ fromJust (makeMove (Move (Piece Pawn Black True) (4,7) (3,7)) startBoard)
      assertBool "Queenside castle while in check" (isNothing (castle King Black inCheck))
      assertBool "Queenside castle onto unsafe squares" (isNothing (castle King Black unSafe))
      assertBool "Queenside castle with moved pieces" (isNothing (castle King Black moved))
      let afterCastle = fromJust $ castle King White startBoard
      assertEqual "King has moved after castling" (lookupB (2,7) afterCastle) (Occupied$Piece King Black True)
      assertEqual "Rook has moved after castling" (lookupB (3,7) afterCastle) (Occupied$Piece Rook Black True)
  ]


tests :: Test
tests = TestList [
    TestLabel "testLookup" testLookup,
    TestLabel "testClear" testClear,
    TestLabel "testPlace" testPlace,
    TestLabel "testMove" testMove,
    TestLabel "getMovesPawn" testGetMovesPawn,
    TestLabel "testPromotion" testPromotion,
    TestLabel "castleTest" testCastle
    ]