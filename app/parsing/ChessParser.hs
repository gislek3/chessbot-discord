{-# LANGUAGE OverloadedStrings #-}

module Parsing.ChessParser (module Parsing.ChessParser) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import Chess.Board (Square, isValidSquare)

--Define parser type; using Void for simplicity as we don't have custom error types here.
type Parser = Parsec Void Text

--Parsing results are ChessCommand
data ChessCommand
  = MoveCmd Square Square
  | FlipCmd
  | ResignCmd
  | ShowCmd
  deriving (Show, Eq)


fileToNum :: Char -> Int
fileToNum file = fromEnum file - fromEnum 'a'

squareParser :: Parser Square
squareParser = do
  fileChar <- letterChar
  rankChar <- digitChar
  let file = fromEnum fileChar - fromEnum 'a'  -- 'a' becomes 0, ..., 'h' becomes 7
  let rank = read [rankChar] - 1               -- '1' becomes 0, ..., '8' becomes 7
  if isValidSquare (file, rank)
    then return (file, rank)                   -- Return as (file, rank), bottom-left origin
    else fail "Square out of bounds"

--Move commands should be in the form "square square", such as "e2 e4"
moveParser :: Parser ChessCommand
moveParser = do
  start <- squareParser
  space
  end <- squareParser
  return $ MoveCmd start end

-- A parser for the "flip" command.
flipParser :: Parser ChessCommand
flipParser = string "flip" >> return FlipCmd

-- A parser for the "resign" command.
resignParser :: Parser ChessCommand
resignParser = string "resign" >> return ResignCmd

-- A parser for the "resign" command.
showParser :: Parser ChessCommand
showParser = string "show" >> return ResignCmd

-- Combine all parsers to parse any of the commands.
commandParser :: Parser ChessCommand
commandParser = try moveParser <|> try flipParser <|> resignParser

-- Pparse input text into a ChessCommand.
parseInput :: Text -> Either (ParseErrorBundle Text Void) ChessCommand
parseInput = parse commandParser ""