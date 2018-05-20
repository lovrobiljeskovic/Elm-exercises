{-# LANGUAGE OverloadedStrings #-} -- it is a pragma, this one converts String as Text, more efficient
{-# LANGUAGE DeriveGeneric #-} -- this pragma

module Main where

import Network.Wai.Middleware.Cors --takes care of the same origin problem
import Web.Scotty

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)


import Prelude hiding (id) -- preventing name clashes

import Data.IntMap (IntMap) -- import the type, we use it in the insertMember signature
import qualified Data.IntMap.Strict as IntMap -- import the name space, use it in our insertMember function

import Control.Concurrent ( newMVar , readMVar , takeMVar, putMVar )

import Control.Monad.Trans.Class (lift)

import Network.HTTP.Types.Status

data Member = Member -- the left part is a data Type, the right Member is the constructor
  { id :: Int
  , name :: String
  , email :: String
  } deriving (Show, Generic)

instance ToJSON Member -- automatically jsonify Member when required (when we use json $ IntMap.elems members, each member gets jsonified)

instance FromJSON Member

main :: IO ()
main = do
  membersRef <- newMVar $ IntMap.fromList [(1, Member 1 "Kurt" "kurt@mail")
                                          ,(2, Member 2 "Sonja" "Son@mail")
                                          ]
  scotty 9000 $ do
    middleware simpleCors
    get "/hello/:name" $ do
      name <- param "name"
      html $ mconcat [ "<h1>Hello ", name, " from Scotty!</h1><hr/>"]
    get "/member/count" $ do
      members <- lift $ readMVar membersRef
      json $ IntMap.size members -- json is just another response body function like html, haskell.scotty documentation
    get "/member" $ do
      members <- lift $ readMVar membersRef
      json $ IntMap.elems members
    get "/member/:id" $ do
      idText <- param "id"
      members <- lift $ readMVar membersRef
      let id = (read idText) :: Int -- convert idText into a text
      case IntMap.lookup id members of
        Just member ->
          json member
        Nothing ->
          status status400
    post "/member" $ do
      oldMembers <- lift $ takeMVar membersRef
      member <- jsonData :: ActionM Member
      let (updatedMember, newMembers) = insertMember member oldMembers
      lift $ putMVar membersRef newMembers
      json updatedMember



insertMember :: Member  -> IntMap Member -> (Member , IntMap Member)
insertMember member intMap
  | IntMap.member (id member) intMap == True = (member , IntMap.insert (id member) member intMap) --IntMap.member is a function return True if the provided id exists already in the provided IntMap
  | otherwise  = let newMember = Member (IntMap.size intMap + 1) (name member) (email member)
                in (newMember, IntMap.insert (id newMember) newMember intMap)
