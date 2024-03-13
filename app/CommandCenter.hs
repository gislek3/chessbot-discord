{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module CommandCenter where


{-
IDEA:

Okay now main doesnt deal with chess logic.... But, what if the CommandCenter doesn't care either?

I have a concept for this general section:

- this file (which might be renamed) takes user-input and TRIES to create ready-interpreted commands,
like "move e2 e4" or "flip board" or "resign" (let's pretend these are type-wrapped)
- There SHOULD MAYBE be an additional "parsing file", i.e. Parser.hs, that does all the parsing logic
- another file, GameHandler.hs, that maps users to games and recieves these already-parsed commands
and modifies the game state for that specific user's game based on it. This is the only file that knows
the rules of the game and does any modification to the actual chess boards.

-}





--Local imports

--Discord imports
import Discord.Types (UserId)

--Other imports
import qualified Data.Text as T


--Split version of discord-haskell's Message, to seperate concerns
type Input = (UserId, T.Text)
 
handle :: Input -> T.Text
handle _ = "OK"