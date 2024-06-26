{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

--Module declaration
module Main (main) where


--Local imports
--Discord imports
import           Discord
import           Discord.Types
import qualified Discord.Requests as R

--Stack imports
import Lib

--Imports not otherwise specified
import Control.Concurrent.STM
import           Control.Monad (when, void)
import           UnliftIO.Concurrent
import           Data.Text (isPrefixOf, toLower, pack, unlines, Text)
import qualified Data.Text as T
import           Data.Maybe (isNothing)
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Chess.Board
import Command.GameHandler
import UnliftIO (liftIO)
import Chess.Game



{-
Most of the code found on this page has been copied from:
https://github.com/discord-haskell/discord-haskell/tree/master

And I've only made modifications to specifically facilitate the chessbot, while the
basic framework of recieving and sending message is just taken from the github. I claim
no rights to it.
-}


-- | A higher-order function that takes a Command-handling function and returns an event handler capable of translating game-related state evalutions to Discord events. 
createEventHandler :: (UserId -> T.Text -> IO CommandResult) -> Event -> DiscordHandler ()
createEventHandler handleCommand = \event -> case event of
  MessageCreate m -> when (isPrivateMsg m && not (fromBot m)) $ do
    let userId = userIdFromMessage m
    let inputText = messageContent m
    result <- liftIO $ handleCommand userId inputText
    let response = case result of
                      CommandResult (Passive Print) msg ChessData{board=b} -> msg <> showB b
                      CommandResult (Modified _) msg ChessData{board=b} -> msg <> showB b
                      CommandResult _ msg _ -> msg

    void $ restCall (R.CreateMessage (messageChannelId m) response)
  _ -> return ()


chessbot :: IO ()
chessbot = do
  gameRegistry <- newTVarIO M.empty
  let handleCommand = setupGameHandler gameRegistry

  userFacingError <- runDiscord $ def
        { discordToken = "Bot MTIwOTk5NTU5NzgwNTkxNjIwMQ.GQcDzY.oH0BmxE2QPdQGOwDUJ0pLlCAZ4GayfLzDUHhQo"
        , discordOnEvent = createEventHandler handleCommand
        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }

  TIO.putStrLn userFacingError



-- | Extract the user ID from a message
userIdFromMessage :: Message -> UserId
userIdFromMessage = userId . messageAuthor

-- | Infers that the author of a given message was a bot by checking its user data
fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

-- | Check if a message is a private message. Useful because the bot should only respond to private messages.
isPrivateMsg :: Message -> Bool
isPrivateMsg m = isNothing (messageGuildId m)


main :: IO ()
main = chessbot