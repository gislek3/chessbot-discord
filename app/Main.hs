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

And I've only made modifications to it to suit my needs.
-}

-- Define a higher-order function that takes a handleCommand function
-- and returns an event handler
createEventHandler :: (UserId -> T.Text -> IO CommandResult) -> Event -> DiscordHandler ()
createEventHandler handleCommand = \event -> case event of
  MessageCreate m -> when (isPrivateMsg m && not (fromBot m)) $ do
    let userId = userIdFromMessage m
    let inputText = messageContent m
    result <- liftIO $ handleCommand userId inputText
    let response = case result of
                      CommandResult (Success Print) msg ChessGame{board=b} -> msg <> showB b
                      CommandResult (Success LegalMove) msg ChessGame{board=b} -> msg <> showB b
                      CommandResult (Success Reset) msg ChessGame{board=b} -> msg <> showB b
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



-- Extract the user ID from a message
userIdFromMessage :: Message -> UserId
userIdFromMessage = userId . messageAuthor

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

-- Check if a message is a private message
isPrivateMsg :: Message -> Bool
isPrivateMsg m = isNothing (messageGuildId m)


--isPing :: Message -> Bool
--isPing = ("ping" `isPrefixOf`) . toLower . messageContent

main :: IO ()
main = chessbot