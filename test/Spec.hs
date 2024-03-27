import Test.HUnit

import ParserTest
import GameTest
import BoardTest

main :: IO ()
main = do
  _ <- runTestTT ParserTest.tests
  _ <- runTestTT BoardTest.tests
  _ <- runTestTT GameTest.tests
  return ()