{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Command.GameHandler where

--Discord imports
import Discord.Types (UserId)

--Other imports
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Text as T

--Local imports
import Parsing.ChessParser (parseInput, ChessCommand(..))
import Chess.Game
import Chess.Piece (Color(..), PieceType(Queen, King))
import Chess.Board (SquareContent(Illegal))


--TVARs from STM to support atomic memory transactions, creating a functional and thread-safe global registry
type GameRegistry = TVar (M.Map UserId ChessGame)

--High-levelled status enum that summarizes the outcome of a ChessCommand
data CommandOutcome = Modified GameState | Passive PassiveType | Unmodified FailType  deriving (Show, Eq)

data SuccessType = LegalMove | Check | Resign deriving (Show, Eq)
data FailType = IllegalMove | Invalid deriving (Show, Eq)
data PassiveType = Print deriving (Show, Eq)

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
processCommand :: ChessCommand -> ChessGame -> CommandResult
processCommand command game =
  case command of
    MoveCmd start end -> let attempt = move start end game in
      if updated attempt then case gameState attempt of
        InCheck White -> CommandResult (Modified Active) "White is in check!" attempt
        InCheck Black -> CommandResult (Modified Active) "Black is in check!" attempt
        CheckMate White -> CommandResult (Modified Active) "Checkmate! Black has won the game." attempt
        CheckMate Black -> CommandResult (Modified Active) "Checkmate! White has won the game." attempt
        Stalemate -> CommandResult (Modified Active) "Stalemate! It's a draw." attempt
        _ -> CommandResult (Modified Active) "" attempt
      else case gameState attempt of
        Active -> CommandResult (Unmodified IllegalMove) "Sorry, that move is not possible." game
        InCheck _ -> CommandResult (Unmodified IllegalMove) "You are still in check! Try a different move." game
        _ -> CommandResult (Unmodified IllegalMove) "The game is over, you can't make any more moves. Type 'reset' to play again, or type 'show' for a list of commands" game 
    
    CastleCmd side -> let attempt = castle side game in
      if updated attempt then
        CommandResult (Modified Active) "Castle successful!" attempt
      else
        CommandResult (Unmodified IllegalMove) "You are unable to castle." attempt
    
    ResignCmd -> CommandResult (Modified Resigned) "Game over, you have resigned." $ resign game
    FlipCmd -> CommandResult (Unmodified Invalid) "That feature is not yet implemented." game
    ShowCmd -> CommandResult (Passive Print) "Here's the current board:" game
    ResetCmd -> CommandResult (Modified Reset) "Okay, I reset the game for you." $ reset game
