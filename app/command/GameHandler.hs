{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Command.GameHandler where

import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Text as T
import Discord.Types (UserId)
import Parsing.ChessParser (parseInput, ChessCommand(..))
import Chess.Board (Board, Move, startingBoard)
import Data.Maybe (isNothing)
import Chess.Game

type GameRegistry = TVar (M.Map UserId ChessGame)
data CommandOutcome = Success | LegalMove | IllegalMove | Invalid deriving (Show, Eq)

-- CommandResult might include the outcome, a message for the user, and optionally the current state of the board.
data CommandResult = CommandResult {
    outcome :: CommandOutcome,
    message :: T.Text,
    game :: ChessGame  -- The current state of the board, if applicable
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
    let game = M.findWithDefault defaultStart userId registry
    -- Attempt to parse the input. If parsing fails, return the current board state with an error message.
    case parseInput inputText of
      Left _ -> return $ CommandResult Invalid "Invalid command" game
      Right command -> do
        let commandResult = processCommand command game
        case commandResult of
          -- Only update the game registry if the command was processed successfully.
          CommandResult Success _ updatedGame -> do
            let updatedRegistry = M.insert userId updatedGame registry
            writeTVar gameRegistry updatedRegistry
            return commandResult
          -- For outcomes other than Success, do not update the registry.
          _ -> return commandResult


processCommand :: ChessCommand -> ChessGame -> CommandResult
processCommand command game =
  case command of
    MoveCmd start end -> let attempt = move start end game in
      if updated attempt
        then CommandResult Success "OK" attempt
      else CommandResult IllegalMove "NOT OK" game

    ResignCmd ->
      -- Handle resignation, possibly resetting the board to starting state or marking the game as finished
      CommandResult Success "Game over, you have resigned." game
    
    _ -> CommandResult Invalid "Invalid command submitted." game