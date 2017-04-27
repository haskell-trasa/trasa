{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
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

routes :: TVar (M.Map Key Person) -> Route cs rq rp -> Rec Identity cs -> RequestBody Identity rq -> IO rp
routes database route captures reqBody = atomically $ case route of
  AddR -> do
    m <- readTVar database
    let newKey = case M.maxViewWithKey m of
                Just ((k,_),_) -> succ k
                Nothing -> Key 0
    case reqBody of
      RequestBodyPresent (Identity person) -> do
        writeTVar database (M.insert newKey person m)
        return newKey
  EditR -> do
    m <- readTVar database
    case captures of
      Identity k :& RNil -> case reqBody of
        RequestBodyPresent (Identity person) -> writeTVar database (M.insert k person m)
  DeleteR -> do
    m <- readTVar database
    case captures of
      Identity k :& RNil -> writeTVar database (M.delete k m)
  ViewR -> do
    m <- readTVar database
    case captures of
      Identity k :& RNil -> case M.lookup k m of
        Just person -> return person
        Nothing -> error "We really need better error handling"
  ViewAllR -> do
    m <- readTVar database
    return (fmap (uncurry Keyed) (M.toList m))

application :: TVar (M.Map Key Person) -> Application
application database =
  serve (metaMethod . meta)
        (mapPath (CaptureDecoding . captureCodecDecode) . metaPath . meta)
        (mapRequestBody (Many . pure . bodyCodecToBodyDecoding) . metaRequestBody . meta)
        (mapResponseBody (Many . pure . bodyCodecToBodyEncoding) . metaResponseBody . meta)
        (routes database)
        allRoutes


main :: IO ()
main = do
  database <- newTVarIO (M.fromList people)
  (run 8080 . logStdoutDev . application) database
  where people = [(Key 0, Person 18 "Kyle"),(Key 1, Person 25 "Drew")]
