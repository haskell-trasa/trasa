{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Trasa.Server
  ( TrasaT
  , runTrasaT
  , serve
  ) where

import Control.Monad (join)
import Data.Traversable (for)
import Data.Functor.Identity
import Data.Vinyl.Core (Rec)

import Network.HTTP.Types.Header (hAccept,hContentType)
import qualified Network.HTTP.Types.Status as S
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

serve
  :: (forall caps qrys req resp. route caps qrys req resp -> Rec (Query CaptureDecoding) qrys)
  -> (forall caps qrys req resp. route caps qrys req resp -> RequestBody (Many BodyDecoding) req)
  -> (forall caps qrys req resp. route caps qrys req resp -> ResponseBody (Many BodyEncoding) resp)
  -> (forall caps qrys req resp.
         route caps qrys req resp
      -> Rec Identity caps
      -> Rec Parameter qrys
      -> RequestBody Identity req
      -> TrasaT IO resp)
  -> Router route -- ^ Router
  -> WAI.Application -- ^ WAI Application
serve toQuerys toReqBody toRespBody makeResponse router =
  \req respond ->
    case TE.decodeUtf8' (WAI.requestMethod req) of
      Left _ -> respond (WAI.responseLBS S.status400 [] "Non utf8 encoded request method")
      Right method -> case parseHeaders req of
        Left _ -> respond (WAI.responseLBS S.status400 [] "Non utf8 encoded headers")
        Right headers -> case parseAccepts headers of
          Nothing -> respond (WAI.responseLBS S.status415 [] "Accept header missing")
          Just accepts -> do
            content <- for (M.lookup hContentType headers) $ \typ ->
              Content typ <$> WAI.strictRequestBody req
            let url = Url (WAI.pathInfo req) (decodeQuery (WAI.queryString req))
                dispatch = dispatchWith toQuerys toReqBody toRespBody makeResponse router method accepts url content
            runTrasaT dispatch headers >>= \case
              (resErr,newHeaders) -> case join resErr of
                Left (TrasaErr stat errBody) ->
                  respond (WAI.responseLBS stat (encodeHeaders newHeaders) errBody)
                Right (Content typ lbs) -> do
                  let encodedHeaders = encodeHeaders (M.insert hContentType typ newHeaders)
                  respond (WAI.responseLBS S.status200 encodedHeaders lbs)
  where
    encodeHeaders = M.toList . fmap TE.encodeUtf8
    parseHeaders = traverse TE.decodeUtf8' . M.fromList . WAI.requestHeaders
    parseAccepts = fmap (fmap T.strip . T.splitOn (T.singleton ',')) . M.lookup hAccept
