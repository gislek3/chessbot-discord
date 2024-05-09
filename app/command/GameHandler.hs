{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text



module Command.GameHandler where

--Discord imports
import Discord.Types (UserId)

--Other imports
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.State (runState)

--Local imports
import Parsing.ChessParser (parseInput, ChessCommand(..))
import Chess.Game
import Chess.Piece (Color(..), PieceType(Queen, King))
import Parsing.ChessParser (ChessCommand(..))


--TVARs from STM to support atomic memory transactions, creating a functional and thread-safe global registry
type GameRegistry = TVar (M.Map UserId ChessData)

--High-levelled status enum that summarizes the outcome of a ChessCommand
data CommandOutcome = Modified GameState | Passive PassiveType | Unmodified FailType  deriving (Show, Eq)

data SuccessType = LegalMove | Check | Resign deriving (Show, Eq)
data FailType = IllegalMove | Invalid deriving (Show, Eq)
data PassiveType = Print | Turn deriving (Show, Eq)

--Composite of a result: The enumeric outcome of the ChessCommand, a human-friendly summary, and the resulting game of chess
data CommandResult = CommandResult {
    outcome :: CommandOutcome,
    message :: T.Text,
    game :: ChessData
} deriving (Show, Eq)

--Create a command handler which recieves user input, parses and executes commands on games
setupGameHandler :: GameRegistry -> UserId -> T.Text -> IO CommandResult
setupGameHandler gameRegistry userId inputText =
  atomically $ do --as we want to read the shared registry, we need an atomic action
    registry <- readTVar gameRegistry --read the registry
    let game = M.findWithDefault defaultStart userId registry --find the user's session

    --use chessparser to decipher the user's input text
    case parseInput inputText of
      Left _ -> return $ CommandResult (Unmodified Invalid) "Invalid command" game --failed to parse
      Right command -> do --successful parse into a ChessCommand
        let commandResult = processCommand command game --result
        case commandResult of
          CommandResult (Modified _) _ updatedGame -> do --only write on success
            let updatedRegistry = M.insert userId updatedGame registry
            writeTVar gameRegistry updatedRegistry --still atomic, so we can write
            return commandResult
          _ -> return commandResult --


--Function which handles the use of ChessCommand on the user's game and communicates result
processCommand :: ChessCommand -> ChessData -> CommandResult
processCommand command input =
  case command of
    MoveCmd start end -> let (success, output) = runState (move start end) input in
      if success then do
        case gameState output of
          InCheck White -> CommandResult (Modified Active) "White is in check!" output
          InCheck Black -> CommandResult (Modified Active) "Black is in check!" output
          CheckMate White -> CommandResult (Modified Active) "Checkmate! Black has won the game." output
          CheckMate Black -> CommandResult (Modified Active) "Checkmate! White has won the game." output
          Stalemate -> CommandResult (Modified Active) "Stalemate! It's a draw." output
          Drawn -> CommandResult (Modified Active) "Well played, it's a draw." output
          Failed -> CommandResult (Modified Active) "Sorry, there's something wrong with me! I was not able to get a move for you. As far as I know you're forced to restart now, but you can try to make a move. I'm sorry that this has happened." output
          _ -> CommandResult (Modified Active) "" output
      else case gameState output of
        Active -> CommandResult (Unmodified IllegalMove) "Sorry, that move is not possible." output
        InCheck _ -> CommandResult (Unmodified IllegalMove) "You are still in check! Try a different move." output
        _ -> CommandResult (Unmodified IllegalMove) "The game is over, you can't make any more moves. Type 'reset' to play again, or type 'show' for a list of commands" output 
    
    CastleCmd side -> let (success, output) = runState (castle side) input in
      if success then
        CommandResult (Modified Active) "Castle successful!" output
      else
        CommandResult (Unmodified IllegalMove) "You are unable to castle." output
    
    TurnCmd -> case toPlay input of
      ON White -> CommandResult (Passive Turn) "It's white's turn to play." input
      ON Black -> CommandResult (Passive Turn) "It's black's turn to play." input
      OFF -> CommandResult (Passive Turn) "It's nobody's turn! The game is over." input
    TakebackCmd -> CommandResult (Unmodified Invalid) "That feature is not yet implemented." input
    ResignCmd -> CommandResult (Modified Resigned) "Game over, you have resigned." input
    FlipCmd -> CommandResult (Unmodified Invalid) "That feature is not yet implemented." input
    ShowCmd -> CommandResult (Passive Print) "Here's the current board:" input
    ResetCmd -> let (success, output) = runState reset input in
      if success then CommandResult (Modified Reset) "Okay, I reset the game for you." output else CommandResult (Unmodified Invalid) "I was for some reason unable to reset the game." input
