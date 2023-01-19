{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Create a common box, store data on many threads, and have 10 userlist.
License     : GPL-3
-}

module Main where

import Types
import Data.List
import Data.Maybe
import System.Random
import Control.Concurrent
import Control.Concurrent (takeMVar)
import Types (User(msgCount))
import System.IO.Unsafe  
import MessagesSent
-- | Make 10 users, put them in an array, then make a thread for each of them and call messages. Once we have an emtpy box, we will terminate the execution using the messagesSent.hs file's Sent method.
main :: IO ()
main = do
    -- For purposes of counting, msg'no is used.
    msg1 <- newMVar 0
    msg2 <- newMVar 0
    msg3 <- newMVar 0
    msg4 <- newMVar 0
    msg5 <- newMVar 0
    msg6 <- newMVar 0
    msg7 <- newMVar 0
    msg8 <- newMVar 0
    msg9 <- newMVar 0
    msg10 <- newMVar 0
    --usr'no are represented by their list of messages and userlist contains all users' names. Each user has a list of 10 unique messages.
    let usr1 = User "Chandrima" msg1 ["This is Chandrima","Hi!","Hello!", "How are you!", "Hope you are doing well!", "How is the weather!", "How is London?", "Is it cold out there?", "How are your classes going?", "How is your health?"]
    let usr2 = User "Chandrani" msg2 ["This is Chandrani","Hi!","Hello","How are you!", "Hope you are doing well!", "How is the weather!", "How is Hyderabad?", "Is it hot out there?", "How is your studies going?", "How is your health?"]
    let usr3 = User "Vikas" msg3 ["This is Vikas","Hi","Hello!","How are you!", "Hope you are doing well!", "How is the weather!", "How is Bangalore?", "Is it foggy out there?", "How is your work going?", "How is your health?"]
    let usr4 = User "Jyoti" msg4 ["This is Jyoti","Hi","Hello!","How are you!", "Hope you are doing well!", "How is the weather!", "How is Kharagpur?", "Is it raining out there?", "What's up in your work life?", "How is your health?"]
    let usr5 = User "Rahul" msg5 ["This is Rahul","Hi","Hello!","How are you!", "Hope you are doing well!", "How is the weather!", "How is Bangalore?", "Is it foggy out there?", "What's up", "How is your health?"]
    let usr6 = User "Kajal" msg6 ["This is Kajal","Hi!!","Hello!","How are you!", "Hope you are doing well!", "How is the weather!", "How is Kharagpur?", "Is it raining out there?", "How is everyone at home?", "How is your health?"]
    let usr7 = User "Mousumi" msg7 ["This is Mousumi","Hi","Hello!","How are you!", "Hope you are doing well!", "How is the weather!", "How is Kharagpur?", "Is it raining out there?", "How is work going?", "How is your health?"]
    let usr8 = User "Seema" msg8 ["This is Seema","Hi","Hello!","How are you!", "Hope you are doing well!", "How is the weather!", "How is Kharagpur?", "Is it foggy out there?", "How is life?", "How is your health?"]
    let usr9 = User "Satya" msg9 ["This is Satya","Hi","Hello!!","How are you!", "Hope you are doing well!", "How is the weather!", "How is Kharagpur?", "Is it cold out there?","What's going on in life?", "How is your health?"]
    let usr10 = User "Dipali" msg10 ["This is Dipali","Hi","Hello!!!","How are you!", "Hope you are doing well!", "How is the weather!", "How is Kharagpur?", "Is it hot out there?", "What's going on?", "How is your health?"]
    let userlist = [usr1, usr2, usr3, usr4, usr5, usr6, usr7, usr8, usr9, usr10]
    messagesCount <- newMVar 0
    stop <- newEmptyMVar
    --For concurrent use, make threads for each user. sadasd
    forkIO $ messagesSent messagesCount usr1 userlist stop
    forkIO $ messagesSent messagesCount usr2 userlist stop
    forkIO $ messagesSent messagesCount usr3 userlist stop
    forkIO $ messagesSent messagesCount usr4 userlist stop
    forkIO $ messagesSent messagesCount usr5 userlist stop
    forkIO $ messagesSent messagesCount usr6 userlist stop
    forkIO $ messagesSent messagesCount usr7 userlist stop
    forkIO $ messagesSent messagesCount usr8 userlist stop
    forkIO $ messagesSent messagesCount usr9 userlist stop
    forkIO $ messagesSent messagesCount usr10 userlist stop
    --When w1 is in the "stop" condition, we can show all user information and counts in accordance with our needs.
    w1 <- takeMVar stop
    print $ "------------------------------------------------------------------------------------------------------------------"
    --The total number of messages a particular user has received is counted using userxcount.
    user1 <- readMVar (msgCount usr1)
    print $ " Messages received by " ++ name usr1 ++ " = " ++ show (user1)
    user2 <- takeMVar (msgCount usr2)
    print $ " Messages received by " ++ name usr2 ++ " = " ++ show (user2)
    user3 <- takeMVar (msgCount usr3)
    print $ " Messages received by " ++ name usr3 ++ " = " ++ show (user3) 
    user4 <- takeMVar (msgCount usr4)
    print $ " Messages received by " ++ name usr4 ++ " = " ++ show (user4) 
    user5 <- takeMVar (msgCount usr5)
    print $ " Messages received by " ++ name usr5 ++ " = " ++ show (user5) 
    user6<- takeMVar (msgCount usr6)
    print $ " Messages received by " ++ name usr6 ++ " = " ++ show (user6) 
    user7<- takeMVar (msgCount usr7)
    print $ " Messages received by " ++ name usr7 ++ " = " ++ show (user7) 
    user8<- takeMVar (msgCount usr8)
    print $ " Messages received by " ++ name usr8 ++ " = " ++ show (user8) 
    user9<- takeMVar (msgCount usr9)
    print $ " Messages received by " ++ name usr9 ++ " = " ++ show (user9)     
    user10<- takeMVar (msgCount usr10)
    print $ " Messages received by " ++ name usr10 ++ " = " ++ show (user10)

    print $ "------------------------------------------------------------------------------------------------------------------"
    print "100 messages have been exchanged between all users."
    print $ "------------------------------------------------------------------------------------------------------------------"
    let finalMsgsCount = [user1, user2, user3, user4, user5, user6, user7, user8, user9, user10]
    --we just converted type(double) to type(int) and put them all in a list -> finalMsgsCount
    
    -- to find the user's name for the person who has been messaged the most.
    let val = maximum finalMsgsCount
    let max_usr = fromJust $ elemIndex val finalMsgsCount
    print $ " Most messages have been received by " ++ name (userlist !! max_usr) ++ " (" ++ show (val) ++")."
    print $ "------------------------------------------------------------------------------------------------------------------"

