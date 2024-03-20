{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Command.GameHandler where

import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Text as T
import Discord.Types (UserId)
import Parsing.ChessParser (parseInput, ChessCommand(..))
import Chess.Board (Board, Move, startingBoard, makeMove')
import Data.Maybe (isNothing)

type GameRegistry = TVar (M.Map UserId Board)
data CommandOutcome = Success | LegalMove | IllegalMove | Invalid deriving (Show, Eq)

-- CommandResult might include the outcome, a message for the user, and optionally the current state of the board.
data CommandResult = CommandResult {
    outcome :: CommandOutcome,
    message :: T.Text,
    board :: Board  -- The current state of the board, if applicable
} deriving (Show, Eq)

{- 
setupGameHandler :: GameRegistry -> (UserId -> T.Text -> IO CommandResult)
setupGameHandler gameRegistry = \userId inputText ->
  case parseInput inputText of
    Left _ -> atomically $ do
      -- Retrieve the current board state to return it unchanged on invalid command
      registry <- readTVar gameRegistry
      let board = M.findWithDefault startingBoard userId registry
      return (Invalid, "Invalid command", board)
    Right command -> atomically $ do
      registry <- readTVar gameRegistry
      -- Here, it's ensured that board always has a state, either the existing state or startingBoard for new users
      let board = M.findWithDefault startingBoard userId registry
      let (outcome, message, newBoard) = processCommand command board
      -- Update the registry with the new or unchanged board
      let updatedRegistry = M.insert userId newBoard registry
      writeTVar gameRegistry updatedRegistry
      return (outcome, message, newBoard) -}


setupGameHandler :: GameRegistry -> (UserId -> T.Text -> IO CommandResult)
setupGameHandler gameRegistry = \userId inputText ->
  atomically $ do
    registry <- readTVar gameRegistry
    let board = M.findWithDefault startingBoard userId registry
    -- Attempt to parse the input. If parsing fails, return the current board state with an error message.
    case parseInput inputText of
      Left _ -> return $ CommandResult Invalid "Invalid command" board
      Right command -> do
        let commandResult = processCommand command board
        case commandResult of
          -- Only update the game registry if the command was processed successfully.
          CommandResult Success _ newBoard -> do
            let updatedRegistry = M.insert userId newBoard registry
            writeTVar gameRegistry updatedRegistry
            return commandResult
          -- For outcomes other than Success, do not update the registry.
          _ -> return commandResult


processCommand :: ChessCommand -> Board -> CommandResult
processCommand command board =
  case command of
    MoveCmd start end ->
      case makeMove' start end board of
        Nothing -> CommandResult IllegalMove "NOT OK" board
        Just newBoard -> CommandResult Success "OK" newBoard
    ResignCmd ->
      -- Handle resignation, possibly resetting the board to starting state or marking the game as finished
      CommandResult Success "Game over, you have resigned." board
    
    _ -> CommandResult Invalid "Invalid command submitted." board