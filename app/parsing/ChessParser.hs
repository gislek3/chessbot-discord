{-# LANGUAGE OverloadedStrings #-}

module Parsing.ChessParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text, pack)
import Chess.Board (Square, isValidSquare)
import Control.Monad (void, when)

-- Define a type for the parser; using Void for simplicity as we don't have custom error types here.
type Parser = Parsec Void Text

data ChessCommand
  = MoveCmd Square Square
  | FlipCmd
  | ResignCmd
  deriving (Show, Eq)


fileToNum :: Char -> Int
fileToNum file = fromEnum file - fromEnum 'a'

squareParser :: Parser Square
squareParser = do
  fileChar <- letterChar
  rankChar <- digitChar
  let file = fromEnum fileChar - fromEnum 'a'  -- 'a' becomes 0, ..., 'h' becomes 7
  let rank = read [rankChar] - 1  -- '1' becomes 0, ..., '8' becomes 7
  if isValidSquare (file, rank)
    then return (file, rank)  -- Return as (file, rank), aligning with conventional bottom-left origin
    else fail "Square out of bounds"

-- Updated parser for a chess move
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

-- Combine all parsers to parse any of the commands.
commandParser :: Parser ChessCommand
commandParser = try moveParser <|> try flipParser <|> resignParser

-- Function to parse input text into a ChessCommand.
parseInput :: Text -> Either (ParseErrorBundle Text Void) ChessCommand
parseInput = parse commandParser ""