{-# LANGUAGE OverloadedStrings #-}

module Parsing.ChessParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text, pack)
import Chess.Board (Square)
import Control.Monad (void, when)

-- Define a type for the parser; using Void for simplicity as we don't have custom error types here.
type Parser = Parsec Void Text

data ChessCommand
  = Move Square Square  -- Updated to use Square
  | Flip
  | Resign
  deriving (Show, Eq)


fileToNum :: Char -> Int
fileToNum file = fromEnum file - fromEnum 'a'

isValidSquare :: Int -> Int -> Bool
isValidSquare file rank = file >= 0 && file <= 7 && rank >= 0 && rank <= 7

squareParser :: Parser Square
squareParser = do
  file <- letterChar
  rank <- digitChar
  let fileNum = fileToNum file
      rankNum = read [rank] - 1
  when (not $ isValidSquare fileNum rankNum) $ fail "Square out of bounds"
  return (fileNum, rankNum)

-- Updated parser for a chess move
moveParser :: Parser ChessCommand
moveParser = do
  start <- squareParser
  space
  end <- squareParser
  return $ Move start end

-- A parser for the "flip" command.
flipParser :: Parser ChessCommand
flipParser = string "flip" >> return Flip

-- A parser for the "resign" command.
resignParser :: Parser ChessCommand
resignParser = string "resign" >> return Resign

-- Combine all parsers to parse any of the commands.
commandParser :: Parser ChessCommand
commandParser = try moveParser <|> try flipParser <|> resignParser

-- Function to parse input text into a ChessCommand.
parseInput :: Text -> Either (ParseErrorBundle Text Void) ChessCommand
parseInput = parse commandParser ""