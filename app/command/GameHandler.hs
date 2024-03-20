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

--Composite of a result: The enumeric outcome of the ChessCommand, a human-friendly summary, and the resulting game of chess
data CommandResult = CommandResult {
    outcome :: CommandOutcome,
    message :: T.Text,
    game :: ChessGame 
} deriving (Show, Eq)


setupGameHandler :: GameRegistry -> (UserId -> T.Text -> IO CommandResult)
setupGameHandler gameRegistry = \userId inputText ->
  atomically $ do --as we want to read the registry, we need an atomic action
    registry <- readTVar gameRegistry --read the registry
    let game = M.findWithDefault defaultStart userId registry --find the user's session

    --use chessparser to decipher the user's input text
    case parseInput inputText of
      Left _ -> return $ CommandResult Invalid "Invalid command" game --failed to parse
      Right command -> do --successful parse into a ChessCommand
        let commandResult = processCommand command game --result
        case commandResult of
          CommandResult Success _ updatedGame -> do --only write on success
            let updatedRegistry = M.insert userId updatedGame registry
            writeTVar gameRegistry updatedRegistry --we still have exclusive access
            return commandResult
          _ -> return commandResult --


--Function which handles the use of ChessCommand on the user's game and communicates result
processCommand :: ChessCommand -> ChessGame -> CommandResult
processCommand command game =
  case command of
    MoveCmd start end -> let attempt = move start end game in
      if updated attempt
        then CommandResult Success "OK" attempt
      else CommandResult IllegalMove "NOT OK" game

    ResignCmd -> CommandResult Success "Game over, you have resigned." game
    _ -> CommandResult Invalid "Invalid command submitted." game
