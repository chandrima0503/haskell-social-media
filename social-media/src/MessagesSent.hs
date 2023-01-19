{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : messagesSent
Description : To stop the execution, count the total number of messages and update the box's status.
License     : GPL-3
-}

module MessagesSent
    ( 
        messagesSent
    ) where

import Types
import System.Random
import Control.Concurrent
import Control.Concurrent (takeMVar)
import Types (User(msgCount))
import System.IO.Unsafe  
-- | messagesSent function is called to count the total number of messages and to complete the function once the box contains 'Flaged'. 
messagesSent messagesCount sender userlist stop = do
    totalMsgs <- takeMVar messagesCount
    -- Check to see if there are less than 100 messages.
    if totalMsgs < 100 then do
        -- That IO provides an IO Int, so in order to obtain Int, we use unsafePerformIO.
        let n = (unsafePerformIO (getStdRandom (randomR (0, 9)))) :: Int
        let user = userlist !! n
        -- Redo if the user and sender are the same.
        if user == sender then do
            putMVar messagesCount totalMsgs
            messagesSent messagesCount sender userlist stop
        else do
            -- To ensure clarity, it is important to identify the sender, recipient, and nature of the message.
            let msgn = (unsafePerformIO (getStdRandom (randomR (0, 9)))) :: Int
            let msgsender = msgsArray sender !! msgn
            print $ "Message : " ++ (msgsender) ++ " || sent by - " ++ (name sender) ++ " || received by - " ++ (name user)
            -- In order to tally the user-specific message count.
            count <- takeMVar (msgCount user)
            -- To increase the number of user-specific messages.
            let counter = count+1
            -- We updated the MVar with the new count.
            putMVar (msgCount user) counter
            let msgcount = totalMsgs+1
            -- Closing the MVar
            putMVar messagesCount msgcount 
            -- Generate random number for delaying
            delayTime <- randomRIO (10, 100)
            -- Delay in microseconds
            threadDelay delayTime
            -- Call the function recursively
            messagesSent messagesCount sender userlist stop
    else do
        -- Close the MVar
        putMVar messagesCount totalMsgs
        -- Make the 'stop' MVar non-empty
        putMVar stop "Flaged"
