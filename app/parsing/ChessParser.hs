{-# LANGUAGE OverloadedStrings #-}

module Parsing.ChessParser (module Parsing.ChessParser) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import Chess.Board (Square, isValidSquare)
import Data.Char (toLower)
import Chess.Piece (PieceType(Queen, King))

{-
TODO:
- Combine all the "simple parsers" into a single one
-}


--Define parser type; using Void for simplicity as we don't have custom error types here.
type Parser = Parsec Void Text

--Parsing results are ChessCommand
data ChessCommand
  = MoveCmd Square Square
  | CastleCmd PieceType
  | FlipCmd
  | ResignCmd
  | ShowCmd
  | ResetCmd
  deriving (Show, Eq)


fileToNum :: Char -> Int
fileToNum file = fromEnum (toLower file) - fromEnum 'a'

squareParser :: Parser Square
squareParser = do
  fileChar <- choice $ map char' ['a'..'h']  -- Accept 'a'-'h' in any case
  rankChar <- digitChar
  let file = fileToNum fileChar  -- 'a' or 'A' becomes 0, ..., 'h' or 'H' becomes 7
  let rank = read [rankChar] - 1 -- '1' becomes 0, ..., '8' becomes 7
  if isValidSquare (file, rank)
    then return (file, rank)     -- Return as (file, rank), bottom-left origin
    else fail "Square out of bounds"

--Move commands should be in the form "square square", such as "e2 e4"
moveParser :: Parser ChessCommand
moveParser = do
  start <- squareParser
  space
  end <- squareParser
  return $ MoveCmd start end

--Castle commands should come in the form "castle queenside/kingside"
castleParser :: Parser ChessCommand
castleParser = do
  _ <- string' "castle"
  space
  side <- choice 
    [ Queen <$ string' "queen"
    , King <$ string' "king"
    ]
  return $ CastleCmd side

-- A parser for the "flip" command.
flipParser :: Parser ChessCommand
flipParser = string' "flip" >> return FlipCmd

-- A parser for the "resign" command.
resignParser :: Parser ChessCommand
resignParser = string' "resign" >> return ResignCmd

-- A parser for the "show" command.
showParser :: Parser ChessCommand
showParser = (string' "show" <|> string' "print") >> return ShowCmd

--A parser for the "reset" command
resetParser :: Parser ChessCommand
resetParser = (string' "reset" <|> string' "start over") >> return ResetCmd

-- Combine all parsers to parse any of the commands.
commandParser :: Parser ChessCommand
commandParser = try moveParser <|> try flipParser <|> resignParser <|> showParser <|> resetParser <|> castleParser

-- Pparse input text into a ChessCommand.
parseInput :: Text -> Either (ParseErrorBundle Text Void) ChessCommand
parseInput = parse commandParser ""