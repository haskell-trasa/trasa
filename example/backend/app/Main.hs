{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Control.Concurrent.STM
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity(..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Topaz.Rec (Rec(..))
import Trasa.Core
import Trasa.Server
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Types.Status as S

routes
  :: forall cs qs rq rp
  .  TVar (M.Map Key Person)
  -> Route cs qs rq rp
  -> Rec Identity cs
  -> Rec Parameter qs
  -> RequestBody Identity rq
  -> TrasaT IO rp
routes database route captures querys reqBody = case route of
  AddR -> go handleAddR
  EditR -> go handleEditR
  DeleteR -> go handleDeleteR
  ViewR -> go handleViewR
  ViewAllR -> go handleViewAllR
  where
  go :: (TVar (M.Map Key Person) -> Arguments cs qs rq (TrasaT IO rp)) -> TrasaT IO rp
  go f = handler captures querys reqBody (f database)

handleAddR :: TVar (M.Map Key Person) -> Person -> TrasaT IO Key
handleAddR database person = liftIO . atomically $ do
  m <- readTVar database
  let newKey = case M.maxViewWithKey m of
        Just ((k,_),_) -> succ k
        Nothing -> Key 0
  writeTVar database (M.insert newKey person m)
  return newKey

handleEditR :: TVar (M.Map Key Person) -> Key -> Person -> TrasaT IO ()
handleEditR database k person = liftIO . atomically $ do
  m <- readTVar database
  writeTVar database (M.insert k person m)

handleDeleteR :: TVar (M.Map Key Person) -> Key  -> TrasaT IO ()
handleDeleteR database k = liftIO . atomically $ do
  m <- readTVar database
  writeTVar database (M.delete k m)

handleViewR :: TVar (M.Map Key Person) -> Key -> TrasaT IO Person
handleViewR database k = do
  m <- liftIO (readTVarIO database)
  case M.lookup k m of
    Just person -> return person
    Nothing -> throwError (TrasaErr S.status404 "Person not found")

handleViewAllR :: TVar (M.Map Key Person) -> Maybe Int -> TrasaT IO [Keyed Person]
handleViewAllR database limit = liftIO . atomically $ do
  m <- readTVar database
  (return . fmap (uncurry Keyed) . maybe id take limit . M.toList) m

router :: Router Route
router = routerWith (mapMeta captureDecoding captureDecoding id id . meta) allRoutes

application :: TVar (M.Map Key Person) -> Application
application database = serveWith
  (metaCodecToMetaServer . meta)
  (routes database)
  router

-- | Example usage:
-- >  curl -v -H 'Content-Type:text/haskell' -X PUT 127.0.0.1:8080/edit/1 -d 'Person { personAge=5,personName="Bob" }'
main :: IO ()
main = do
  database <- newTVarIO (M.fromList people)
  (run 8080 . logStdoutDev . application) database
  where people = [(Key 0, Person 18 "Kyle"),(Key 1, Person 25 "Drew")]
