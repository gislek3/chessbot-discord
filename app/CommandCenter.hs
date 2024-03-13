{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module CommandCenter where


{-
IDEA:
- another file, GameHandler.hs, that maps users to games and recieves these already-parsed commands
and modifies the game state for that specific user's game based on it.
- gamehandler communicates mostly with something like ChessGame.hs that is a wrapped for a board with its accomp
game state, i.e. whose turn it is and if the game is over and so on.
- the chessgame will infer which legal moves (given a piece and a board?) maybe here we need the Reader monad!?
- the GameHandler will thus not know the rules (only ChessGame.hs does) but knows what to ask for
- ChessGame.hs is placed in src, as it relates to the backend. GameHandler.hs needs to be in app, as it
faciliates user-game communication.

So basically:
- Board.hs is just a board in the physical sense, which has pieces on it which can be moved at will (humans know rules)
- ChessGame.hs is the human element, where a "knower of rules" interacts with this board, infers legal moves, checks and so on
- GameHandler.hs is a mapping between a game of chess and the user playing it. It takes high-level commands and translates
it into low-level commands that will be issued to ChessGame.hs
- CommandCenter.hs will send ChessCommands to GameHandler.hs which will, on behalf of the UserID, interact with a
given ChessGame.hs which will contain a Board.hs
-}



--Local imports
import Chess.Board (Board, Square, Move)
import Parsing.ChessParser

--Discord imports
import Discord.Types (UserId)

--Other imports
import qualified Data.Text as T
import Data.Map (lookup)
import qualified Data.Map as M


--Split version of discord-haskell's Message, to seperate concerns
type Input = (UserId, T.Text)

 
-- Converts a Square to Text representation
squareToText :: Square -> T.Text
squareToText (file, rank) = T.pack $ "(" ++ show file ++ "," ++ show rank ++ ")"

-- Converts a ChessCommand to Text
commandToText :: ChessCommand -> T.Text
commandToText (Move start end) = squareToText start <> " " <> squareToText end
commandToText Flip = "Flip command issued"
commandToText Resign = "Resign command issued"


-- Main handler function
handle :: Input -> T.Text
handle (_, commandText) = 
    case parseInput commandText of
        Left _ -> "Invalid input"
        Right command -> do
            commandToText command
