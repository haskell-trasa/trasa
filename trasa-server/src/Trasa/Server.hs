{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-unticked-promoted-constructors #-}
module Trasa.Server
  ( TrasaT
  , Clarify
  , runTrasaT
  , serveWith
  ) where

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
import qualified Network.HTTP.Media.MediaType as N
import Control.Monad.Reader (ReaderT,runReaderT,MonadReader(..),MonadTrans(..))
import Control.Monad.Except (ExceptT,runExceptT,MonadError(..),MonadIO(..))
import Control.Monad.State.Strict (StateT,runStateT,MonadState(..))
import Data.Kind (Type)

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
runTrasaT trasa headers = (flip runReaderT headers . flip runStateT M.empty . runExceptT . unTrasaT) trasa

-- should probably move this into the core trasa library
type family Clarify (c :: Clarity r) where
  Clarify (Clear a) = a
  Clarify (Raw :: Clarity r) = r

serveWith ::
     forall (route :: [Type] -> [Param] -> Bodiedness -> Clarity WAI.Response -> Type)
   . (forall caps qrys req resp. route caps qrys req resp -> MetaServer caps qrys req resp)
  -> (forall caps qrys req (resp :: Clarity WAI.Response).
         route caps qrys req resp
      -> Rec Identity caps
      -> Rec Parameter qrys
      -> RequestBody Identity req
      -> TrasaT IO (Clarify resp))
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
            case parseWith toMeta madeRouter method url content of
              Left (TrasaErr stat errBody) ->
                respond (WAI.responseLBS stat (encodeHeaders headers) errBody)
              Right (Concealed route path querys reqBody) -> case metaResponseBody (toMeta route) of
                ResponseBodyClear _ ->  do
                  (eresp,newHeaders) <- runTrasaT (makeResponse route path querys reqBody) headers
                  case eresp of
                    Left (TrasaErr stat errBody) ->
                      respond (WAI.responseLBS stat (encodeHeaders newHeaders) errBody)
                    Right theResp -> case encodeResponseBody accepts (metaResponseBody (toMeta route)) theResp of
                      Left (TrasaErr stat errBody) ->
                        respond (WAI.responseLBS stat (encodeHeaders newHeaders) errBody)
                      Right (Content typ lbs) -> do
                        let cType = TE.decodeUtf8 (N.renderHeader typ)
                            encodedHeaders = encodeHeaders (M.insert hContentType cType newHeaders)
                        respond (WAI.responseLBS N.status200 encodedHeaders lbs)
                ResponseBodyRaw _ -> do
                  (eresp,newHeaders) <- runTrasaT (makeResponse route path querys reqBody) headers
                  case eresp of
                    Left (TrasaErr stat errBody) ->
                      respond (WAI.responseLBS stat (encodeHeaders newHeaders) errBody)
                    Right theResp -> respond theResp
  where
    encodeHeaders = M.toList . fmap TE.encodeUtf8
    parseHeaders = traverse TE.decodeUtf8' . M.fromList . WAI.requestHeaders
    parseAccepts :: M.Map (CI BS.ByteString) T.Text -> Maybe [N.MediaType]
    parseAccepts headers = do
      accept <- M.lookup hAccept headers
      (traverse N.parseAccept . fmap (TE.encodeUtf8 . T.dropAround (' '==)) . T.splitOn ",") accept


