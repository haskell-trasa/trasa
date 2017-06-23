{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Trasa.Server
  ( TrasaT
  , runTrasaT
  , serveWith
  ) where

import Control.Monad (join)
import Data.Traversable (for)
import Data.Functor.Identity

import Network.HTTP.Types.Header (hAccept,hContentType)
import qualified Network.HTTP.Types.Status as N
import qualified Network.HTTP.Media.Accept as N
import qualified Network.HTTP.Media.RenderHeader as N
import Data.CaseInsensitive (CI)
import qualified Network.Wai as WAI
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as M
import Control.Monad.Reader (ReaderT,runReaderT,MonadReader(..),MonadTrans(..))
import Control.Monad.Except (ExceptT,runExceptT,MonadError(..),MonadIO(..))
import Control.Monad.State.Strict (StateT,runStateT,MonadState(..))

import Trasa.Core

type Headers = M.Map (CI BS.ByteString) T.Text

newtype TrasaT m a = TrasaT
  { unTrasaT :: ExceptT TrasaErr (StateT Headers (ReaderT Headers m)) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadError TrasaErr
  , MonadIO
  , MonadState (M.Map (CI BS.ByteString) T.Text)
  , MonadReader (M.Map (CI BS.ByteString) T.Text))

instance MonadTrans TrasaT where
  lift = TrasaT . lift . lift . lift

runTrasaT
  :: TrasaT m a
  -> M.Map (CI BS.ByteString) T.Text -- ^ Headers
  -> m (Either TrasaErr a, M.Map (CI BS.ByteString) T.Text)
runTrasaT trasa headers = (flip runReaderT headers . flip runStateT M.empty . runExceptT  . unTrasaT) trasa

serveWith
  :: (forall caps qrys req resp. route caps qrys req resp -> MetaServer caps qrys req resp)
  -> (forall caps qrys req resp.
         route caps qrys req resp
      -> Rec Identity caps
      -> Rec Parameter qrys
      -> RequestBody Identity req
      -> TrasaT IO resp)
  -- ^ Actions to perform when requests come in
  -> Router route -- ^ Router
  -> WAI.Application -- ^ WAI Application
serveWith toMeta makeResponse madeRouter =
  \req respond ->
    case decodeMethod <$> TE.decodeUtf8' (WAI.requestMethod req) of
      Left _ -> respond (WAI.responseLBS N.status400 [] "Non utf8 encoded request method")
      Right method -> case parseHeaders req of
        Left _ -> respond (WAI.responseLBS N.status400 [] "Non utf8 encoded headers")
        Right headers -> case parseAccepts headers of
          Nothing -> respond (WAI.responseLBS N.status415 [] "Accept header missing or malformed")
          Just accepts -> do
            content <- for (M.lookup hContentType headers >>= N.parseAccept . TE.encodeUtf8) $ \typ ->
              Content typ <$> WAI.strictRequestBody req
            let url = Url (WAI.pathInfo req) (decodeQuery (WAI.queryString req))
                dispatch = dispatchWith toMeta makeResponse madeRouter method accepts url content
            runTrasaT dispatch headers >>= \case
              (resErr,newHeaders) -> case join resErr of
                Left (TrasaErr stat errBody) ->
                  respond (WAI.responseLBS stat (encodeHeaders newHeaders) errBody)
                Right (Content typ lbs) -> do
                  let cType = TE.decodeUtf8 (N.renderHeader typ)
                      encodedHeaders = encodeHeaders (M.insert hContentType cType newHeaders)
                  respond (WAI.responseLBS N.status200 encodedHeaders lbs)
  where
    encodeHeaders = M.toList . fmap TE.encodeUtf8
    parseHeaders = traverse TE.decodeUtf8' . M.fromList . WAI.requestHeaders
    parseAccepts headers = do
      accept <- M.lookup hAccept headers
      (traverse N.parseAccept . fmap (TE.encodeUtf8 . T.dropAround (' '==)) . T.splitOn ",") accept
