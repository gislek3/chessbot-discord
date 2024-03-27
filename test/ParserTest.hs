{-# LANGUAGE OverloadedStrings #-}

module ParserTest (tests) where

import Test.HUnit
import Parsing.ChessParser (parseInput, ChessCommand(..))

import TestHelpers (isParseError)


testParseMoveCmd :: Test
testParseMoveCmd = TestList
  [ TestCase $ assertEqual "Parsing e2 e4 into MoveCmd" (Right (MoveCmd (4,1) (4,3))) (parseInput "e2 e4")
  , TestCase $ assertEqual "Parsing a1 b1 into MoveCmd" (Right (MoveCmd (0,0) (1,0))) (parseInput "a1 b1")
  , TestCase $ assertEqual "Parsing h8 g8 into MoveCmd" (Right (MoveCmd (7,7) (6,7))) (parseInput "h8 g8")
  , TestCase $ assertEqual "Parsing d5 e5 into MoveCmd" (Right (MoveCmd (3,4) (4,4))) (parseInput "d5 e5")
  , TestCase $ assertBool "Parsing a1 a1 (same square) still converts" $ not (isParseError (parseInput "a1 a1"))
  , TestCase $ assertBool "Parsing gibberish fails" (isParseError (parseInput "hello world!"))
  , TestCase $ assertBool "Parsing legal square to gibberish fails" (isParseError (parseInput "e2 :-)"))
  , TestCase $ assertBool "Parsing legal square to a9 (OOB) fails" (isParseError (parseInput "a8 a9"))
  , TestCase $ assertBool "Parsing OOB to legal square fails" (isParseError (parseInput "a9 a8"))
  , TestCase $ assertBool "Parsing gibberish to legal square fails" (isParseError (parseInput "s++s a4"))
  ]


testSimpleCmds :: Test
testSimpleCmds = TestCase $ do
    let flip_in = "flip"
    let resign_in = "resign"
    let flip_out = Right FlipCmd
    let resign_out = Right ResignCmd
    assertEqual "Converting the 'flip' input into a valid Cmd" flip_out (parseInput flip_in)
    assertEqual "Converting the 'resign' input into a valid Cmd" resign_out (parseInput resign_in)


tests :: Test
tests = TestList [
    TestLabel "testMoveCmd" testParseMoveCmd,
    TestLabel "testSimpleCmds" testSimpleCmds
    ]