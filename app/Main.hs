{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Main (main) where

import           Control.Monad (when, void)
import           UnliftIO.Concurrent
import           Data.Text (isPrefixOf, toLower, pack, unlines, Text)
import           Data.Maybe (isNothing)
import qualified Data.Text.IO as TIO

import           Discord
import           Discord.Types
import qualified Discord.Requests as R

import Lib

{-
Most of the code found on this page has been copied from:
https://github.com/discord-haskell/discord-haskell/tree/master

And I've only made modifications to it to suit my needs.
-}

chessbot :: IO ()
chessbot = do
    userFacingError <- runDiscord $ def
             { discordToken = "Bot MTIwOTk5NTU5NzgwNTkxNjIwMQ.GQcDzY.oH0BmxE2QPdQGOwDUJ0pLlCAZ4GayfLzDUHhQo"
             , discordOnEvent = eventHandler
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (isPrivateMsg m && isPing m && not (fromBot m)) $ do
        threadDelay (2 * 10^6) -- 2-second delay
        void $ restCall (R.CreateMessage (messageChannelId m) startingBoard)
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

-- Check if a message is a private message
isPrivateMsg :: Message -> Bool
isPrivateMsg m = isNothing (messageGuildId m)

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent


{-
Converts a Board int
-}

startingBoard :: Text
startingBoard = Data.Text.unlines
  [ Data.Text.pack "\x2656\x2658\x2657\x2655\x2654\x2657\x2658\x2656"  -- White Rooks, Knights, Bishops, Queen, King, Bishops, Knights, Rooks
  , Data.Text.pack $ replicate 8 '\x2659'  -- White Pawns
  , Data.Text.pack $ replicate 8 '*'       -- Empty Squares
  , Data.Text.pack $ replicate 8 '*'
  , Data.Text.pack $ replicate 8 '*'
  , Data.Text.pack $ replicate 8 '*'
  , Data.Text.pack $ replicate 8 '\x265F'  -- Black Pawns
  , Data.Text.pack "\x265C\x265E\x265D\x265B\x265A\x265D\x265E\x265C"  -- Black Rooks, Knights, Bishops, Queen, King, Bishops, Knights, Rooks
  ]

main :: IO ()
main = chessbot