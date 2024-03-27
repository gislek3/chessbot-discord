{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Command.GameHandler where

--Discord imports
import Discord.Types (UserId)

--Other imports
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (isNothing)

--Local imports
import Parsing.ChessParser (parseInput, ChessCommand(..))
import Chess.Board (Board, startingBoard, Move)
import Chess.Game


--TVARs from STM to support atomic memory transactions, creating a functional and thread-safe global registry
type GameRegistry = TVar (M.Map UserId ChessGame)

--High-levelled status enum that summarizes the outcome of a ChessCommand
data CommandOutcome = Success | LegalMove | IllegalMove | Invalid deriving (Show, Eq)

--Composite of a result: The enumeric outcome of the ChessCommand, a human-friendly summary, and the resulting game of chess
data CommandResult = CommandResult {
    outcome :: CommandOutcome,
    message :: T.Text,
    game :: ChessGame 
} deriving (Show, Eq)

--Create a command handler which recieves user input, parses and executes commands on games
setupGameHandler :: GameRegistry -> (UserId -> T.Text -> IO CommandResult)
setupGameHandler gameRegistry = \userId inputText ->
  atomically $ do --as we want to read the shared registry, we need an atomic action
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
            writeTVar gameRegistry updatedRegistry --still atomic, so we can write
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
