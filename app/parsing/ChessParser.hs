{-# LANGUAGE OverloadedStrings #-}

module Parsing.ChessParser (parseInput, ChessCommand(..)) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import Chess.Board (Square, isValidSquare)
import Data.Char (toLower)
import Chess.Piece (PieceType(Queen, King))


{-
Attempts to parse raw input from user into ChessCommands using MegaParsec. It turns
for example user input like "e2 e4" into a command MoveCmd (4,1) (4,3) which can then
be passed to the GameHandler, which will then in turn pass it to a move-function with these
squares as an argument, which will manipulate the board of the specific user by moving whatever
is on e2 to e4, if the move is possible.
-}


-- | Define parser type; using Void for simplicity and/or tradition.
type Parser = Parsec Void Text

-- | Parsing results are ChessCommand
data ChessCommand
  = MoveCmd Square Square
  | CastleCmd PieceType
  | FlipCmd
  | ResignCmd
  | ShowCmd
  | ResetCmd
  | TurnCmd
  | TakebackCmd
  deriving (Show, Eq)

-- | Converts an intepreted file character to a numerical value
fileToNum :: Char -> Int
fileToNum file = fromEnum (toLower file) - fromEnum 'a'

-- | Parses a valid single square into an integer tuple, representing its rank and file values.
squareParser :: Parser Square
squareParser = do
  fileChar <- choice $ map char' ['a'..'h']  -- Accept 'a'-'h' in any case
  rankChar <- digitChar
  let file = fileToNum fileChar  -- 'a' or 'A' becomes 0, ..., 'h' or 'H' becomes 7
  let rank = read [rankChar] - 1 -- '1' becomes 0, ..., '8' becomes 7
  if isValidSquare (file, rank)
    then return (file, rank)     -- Return as (file, rank), bottom-left origin
    else fail "Square out of bounds"

-- | Move commands should be in the form "square square", such as "e2 e4"
moveParser :: Parser ChessCommand
moveParser = do
  start <- squareParser
  space
  MoveCmd start <$> squareParser

-- | Castle commands should come in the form "castle queenside/kingside"
castleParser :: Parser ChessCommand
castleParser = do
  _ <- string' "castle"
  space
  side <- choice
    [ Queen <$ string' "queen"
    , King <$ string' "king"
    ]
  return $ CastleCmd side


{-
TODO:

Convert these simple parsing functions into a simpler (higher-order?) function which
takes in a list of matching inputs, let's say ["show", "print"] or ["turn", "who"] and
the matching Command-type, respectively turnParser and showParser.
-}

-- | A parser for the "flip" command.
takebackParser :: Parser ChessCommand
takebackParser = (string' "takeback" <|> string' "undo" <|> string' "regret") >> return TakebackCmd

-- | A parser for the "flip" command.
turnParser :: Parser ChessCommand
turnParser = (string' "turn" <|>  string' "who") >> return TurnCmd

-- | A parser for the "flip" command.
flipParser :: Parser ChessCommand
flipParser = string' "flip" >> return FlipCmd

-- | A parser for the "resign" command.
resignParser :: Parser ChessCommand
resignParser = string' "resign" >> return ResignCmd

-- | A parser for the "show" command.
showParser :: Parser ChessCommand
showParser = (string' "show" <|> string' "print") >> return ShowCmd

-- | A parser for the "reset" command
resetParser :: Parser ChessCommand
resetParser = (string' "reset" <|> string' "start over") >> return ResetCmd

-- | Combine all parsers to parse any of the commands.
commandParser :: Parser ChessCommand
commandParser = try moveParser <|> try flipParser <|> resignParser <|> showParser <|> resetParser
                <|> castleParser <|> try turnParser <|> try takebackParser

-- | Parse input text into an intepretable ChessCommand, which will decide how we should modify the state of the chess game belonging to the user who submitted the input.
parseInput :: Text -> Either (ParseErrorBundle Text Void) ChessCommand
parseInput = parse commandParser ""