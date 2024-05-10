import Test.HUnit

import ParserTest
import GameTest
import BoardTest
import EvaluationTest

main :: IO ()
main = do
  _ <- runTestTT ParserTest.tests
  _ <- runTestTT BoardTest.tests
  _ <- runTestTT EvaluationTest.tests
  return ()