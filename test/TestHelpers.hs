module TestHelpers (module TestHelpers) where

import Text.Megaparsec (ParseErrorBundle)
import Data.Text (Text)
import Data.Void (Void)
import Parsing.ChessParser (ChessCommand(..))
import Test.HUnit

isParseError :: Either (ParseErrorBundle Text Void) ChessCommand -> Bool
isParseError (Left _) = True
isParseError _ = False

assertNotEqual :: (Eq a) => String -> a -> a -> Assertion
assertNotEqual message expected actual = 
  assertBool message (expected /= actual)