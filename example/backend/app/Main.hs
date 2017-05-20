{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror#-}
module Main where

import Data.Vinyl (Rec(..))
import Data.Functor.Identity (Identity(..))

import Trasa.Core
import Trasa.Server

import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Common

routes :: forall cs rq rp.
     TVar (M.Map Key Person)
  -> Route cs rq rp
  -> Rec Identity cs
  -> RequestBody Identity rq
  -> IO rp
routes database route captures reqBody = atomically $ case route of
  AddR -> go handleAddR
  EditR -> go handleEditR
  DeleteR -> go handleDeleteR
  ViewR -> go handleViewR
  ViewAllR -> go handleViewAllR
  where
  go :: (TVar (M.Map Key Person) -> Arguments cs rq (STM rp)) -> STM rp
  go f = handler captures reqBody (f database)

handleAddR :: TVar (M.Map Key Person) -> Person -> STM Key
handleAddR database person = do
  m <- readTVar database
  let newKey = case M.maxViewWithKey m of
        Just ((k,_),_) -> succ k
        Nothing -> Key 0
  writeTVar database (M.insert newKey person m)
  return newKey

handleEditR :: TVar (M.Map Key Person) -> Key -> Person -> STM ()
handleEditR database k person = do
  m <- readTVar database
  writeTVar database (M.insert k person m)

handleDeleteR :: TVar (M.Map Key Person) -> Key -> STM ()
handleDeleteR database k = do
  m <- readTVar database
  writeTVar database (M.delete k m)

handleViewR :: TVar (M.Map Key Person) -> Key -> STM Person
handleViewR database k = do
  m <- readTVar database
  case M.lookup k m of
    Just person -> return person
    Nothing -> error "We really need better error handling"

handleViewAllR :: TVar (M.Map Key Person) -> STM [Keyed Person]
handleViewAllR database = do
  m <- readTVar database
  return (fmap (uncurry Keyed) (M.toList m))

router :: Router Route
router = routerWith
  (metaMethod . meta)
  (mapPath (CaptureDecoding . captureCodecDecode) . metaPath . meta)
  allRoutes

application :: TVar (M.Map Key Person) -> Application
application database = serve
  (mapRequestBody (Many . pure . bodyCodecToBodyDecoding) . metaRequestBody . meta)
  (mapResponseBody (Many . pure . bodyCodecToBodyEncoding) . metaResponseBody . meta)
  (routes database)
  router

main :: IO ()
main = do
  database <- newTVarIO (M.fromList people)
  (run 8080 . logStdoutDev . application) database
  where people = [(Key 0, Person 18 "Kyle"),(Key 1, Person 25 "Drew")]
