module BoardTest (tests) where

import Test.HUnit
import TestHelpers
import Chess.Board
import Chess.Piece
import Data.Maybe (isJust, fromJust, isNothing)
import qualified Data.Set as S
import qualified Data.Map as M


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


testGetMove :: Test
testGetMove = TestCase $ do
  let allWhiteMoves = S.toList $ getAllColorMoves White startingBoard
  let allBlackMoves = S.toList $ getAllColorMoves Black startingBoard
  assertBool "There should be 20 intended moves at the start" (length allWhiteMoves==20&&length allBlackMoves==20)

  --Various positioned pieces
  let bishop = (Piece Bishop White True, (4,4))
  let queen = (Piece Queen White True, (4,4))
  let knight = (Piece Knight Black True, (7,4))
  let king = (Piece King Black True, (0,0))
  let rook = (Piece Rook Black True, (1,5))

  --These checks also verify that enemy pieces are counted as moves, but friendly pieces aren't 
  assertEqual "Correct number of moves for bishop" (length (S.toList (getMoves bishop empty))) 13
  assertEqual "Correct number of moves for queen" (length (S.toList (getMoves queen startingBoard))) 19
  assertEqual "Correct number of moves for knight" (length (S.toList (getMoves knight startingBoard))) 3
  assertEqual "Correct number of moves for king" (length (S.toList (getMoves king empty))) 3
  assertEqual "Correct number of moves for rook" (length (S.toList (getMoves rook startingBoard))) 11
  
  
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


testPromotion :: Test
testPromotion = TestList [
  TestCase $ do --for white
    let emptyBoard = empty
    let startBoard = place (0,6) (Piece Pawn White True) emptyBoard
    let moveResult = makeMove (Move (Piece Pawn White True) (0,6) (0,7)) startBoard
    assertBool "Move succeeds" (isJust moveResult)
    assertEqual "White pawn has been promoted." (Occupied (Piece Queen White True)) (lookupB (0,7) $ fromJust moveResult)
  
  , TestCase $ do --for black
    let emptyBoard = empty
    let startBoard = place (0,1) (Piece Pawn Black True) emptyBoard
    let moveResult = makeMove (Move (Piece Pawn Black True) (0,1) (0,0)) startBoard
    assertBool "Move succeeds" (isJust moveResult)
    assertEqual "Black pawn has been promoted." (Occupied (Piece Queen Black True)) (lookupB (0,0) $ fromJust moveResult)
  ]

testWhitePawnMovement :: Test
testWhitePawnMovement = TestList [
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

testBlackPawnMovement :: Test
testBlackPawnMovement = TestList [
  TestCase $ do --Normal forward moves work
    let e7pos = (4,6)
    let e7piece = fromJust $ M.lookup e7pos b
    let possibleMoves = getMoves (e7piece, e7pos) b
    let expectedMoves = S.fromList [Move e7piece (4,6) (4,4), Move e7piece (4,6) (4,5)]
    assertEqual "Case #1: Pawn at e7 can move to e6 and e5" expectedMoves possibleMoves,
  TestCase $ do --Attacking diagonals work, and they don't override normal forward moves.
    let e7pos = (4,6)
    let attackablesAdded = place (3,5) (startP Rook White) (place (5,5) (startP Bishop White) b)
    let e2piece = fromJust $ M.lookup e7pos attackablesAdded
    let possibleMoves = getMoves (e2piece, e7pos) attackablesAdded
    let expectedMoves = S.fromList [Move e2piece (4,6) (4,5), Move e2piece (4,6) (4,4), Move e2piece (4,6) (3,5), Move e2piece (4,6) (5,5)]
    assertEqual "Case #2: Pawn at e7 can also attack its diagonals" expectedMoves possibleMoves,
  TestCase $ do --Double move is only valid once
    let e7pos = (4,6)
    let e7piece = fromJust $ M.lookup e7pos b
    let movedOnce = makeMove (Move e7piece (4,6) (4,5)) b
    assertBool "Case #3a: Initial move to e6 is valid" (isJust movedOnce)
    assertBool "Case #3b: Cannot double move after initial" (isNothing $ makeMove (Move (Piece Pawn Black True) (4,5) (4,3)) $ (fromJust movedOnce))
  ]


testCastle :: Test
testCastle = TestList [
    TestCase $ do --Kingside castle as white
      let startBoard = place (4,0) (Piece King White False) (place (7,0) (Piece Rook White False) empty)
      let afterCastle = castleKing King White startBoard
      assertBool "Kingside castle with no obstacles succeeds" (isJust afterCastle)

      let blocked = place (6,0) (Piece Knight White False) startBoard
      let unSafe = place (5,4) (Piece Rook Black True) startBoard
      let moved = fromJust $ makeMove (Move (Piece King White False) (5,0) (4,0)) $ fromJust (makeMove (Move (Piece King White True) (4,0) (5,0)) startBoard)
      
      assertBool "Kingside castle onto unsafe squares fails" (isNothing (castleKing King White unSafe))
      assertBool "Kingside castle with moved pieces fails" (isNothing (castleKing King White moved))
      assertBool "Kingside castle while blocked fails" (isNothing (castleKing King White blocked))
      
      assertEqual "King has moved after castling" (lookupB (6,0) (fromJust afterCastle)) (Occupied$Piece King White True)
      assertEqual "Rook has moved after castling" (lookupB (5,0) (fromJust afterCastle)) (Occupied$Piece Rook White True)

      , TestCase $ do --Queenside castle as black
      let startBoard = place (4,7) (Piece King Black False) (place (0,7) (Piece Rook Black False) empty)
      let afterCastle = castleKing Queen Black startBoard
      assertBool "Queenside castle with no obstacles succeeds" (isJust afterCastle)

      let blocked = place (1,7) (Piece Knight Black False) startBoard
      let unSafe = place (2,4) (Piece Rook White True) startBoard
      let moved = fromJust $ makeMove (Move (Piece King Black True) (3,7) (4,7)) $ fromJust (makeMove (Move (Piece King Black False) (4,7) (3,7)) startBoard)
      
      assertBool "Queenside castle onto unsafe squares fails" (isNothing (castleKing Queen Black unSafe))
      assertBool "Queenside castle with moved pieces fails" (isNothing (castleKing Queen Black moved))
      assertBool "Queenside castle while blocked fails" (isNothing (castleKing Queen Black blocked))
      
      assertEqual "King has moved after castling" (lookupB (2,7) $ fromJust afterCastle) (Occupied$Piece King Black True)
      assertEqual "Rook has moved after castling" (lookupB (3,7) $ fromJust afterCastle) (Occupied$Piece Rook Black True)
  ]


testLegalMoves :: Test
testLegalMoves = TestList [
    TestCase $ do
      let placed = place (3,4) (Piece Rook Black True) empty
      let wKing = place (4,0) (Piece King White True) placed
      let wKingMoves = getAllLegalColorMoves White wKing
      let p = justPiece (lookupB (4,0) wKing)
      let illegal = Move p (4,0) (3,0)
      assertBool "Moved is not classified as legal" (not $ isLegalMove illegal wKing)
      assertBool "Does not consider putting yourself in check" (notElem illegal wKingMoves)
  ]


tests :: Test
tests = TestList [
    TestLabel "testLookup" testLookup,
    TestLabel "testClear" testClear,
    TestLabel "testPlace" testPlace,
    TestLabel "testMove" testMove,
    TestLabel "testGetMove" testGetMove,
    TestLabel "testWhitePawnMovement" testWhitePawnMovement,
    TestLabel "testBlackPawnMovement" testBlackPawnMovement,
    TestLabel "testPromotion" testPromotion,
    TestLabel "testCastle" testCastle,
    TestLabel "testLegalMoves" testLegalMoves
    ]