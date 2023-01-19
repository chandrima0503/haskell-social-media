{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Types
Description : To keep track of user communications, create a user data type that contains their username and a message type.
License     : GPL-3
-}

module Types
    ( 
        User (..)
    ) where

import Control.Concurrent ( MVar )
-- | create user data type that has a name, message count and an array of customised messages for each user.
data User = User {
    name :: String,
    msgCount :: MVar Integer,
    msgsArray :: [String]
} deriving (Eq)