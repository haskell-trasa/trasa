{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror -Wwarn=deprecations #-}

module Trasa.Server
  ( TrasaT
  , TrasaEnv(..)
  , runTrasaT
  , mapTrasaT
  , serveWith
  ) where

import Control.Applicative (liftA2, Alternative(..))
import Control.Monad (join, MonadPlus(..))
import Control.Monad.Except (ExceptT,runExceptT,mapExceptT,MonadError(..),MonadIO(..))
import Control.Monad.Reader (ReaderT,runReaderT,mapReaderT,MonadReader(..),MonadTrans(..))
import Control.Monad.State.Strict (StateT,runStateT,mapStateT,MonadState(..))
import Data.CaseInsensitive (CI)
import Data.Functor.Identity
import Data.IORef
import Data.Traversable (for)
import Network.HTTP.Types.Header (hAccept,hContentType)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Media.Accept as N
import qualified Network.HTTP.Media.MediaType as N
import qualified Network.HTTP.Media.RenderHeader as N
import qualified Network.HTTP.Types.Status as N
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai

import Trasa.Core

type Headers = M.Map (CI BS.ByteString) T.Text

data TrasaEnv = TrasaEnv
  { trasaHeaders :: Headers
  , trasaQueryString :: QueryString
  , trasaRequestBody :: BS.ByteString
  , trasaRequestBodyType :: Maybe Wai.RequestBodyType
  }

newtype TrasaT m a = TrasaT
  { unTrasaT :: ExceptT TrasaErr (StateT Headers (ReaderT TrasaEnv m)) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadError TrasaErr
  , MonadIO
  , MonadState (M.Map (CI BS.ByteString) T.Text)
  , MonadReader TrasaEnv
  )

instance (Monad m, Semigroup a) => Semigroup (TrasaT m a) where
  (<>) = liftA2 (<>)

instance (Monad m, Monoid a) => Monoid (TrasaT m a) where
  mempty = pure mempty

instance (Alternative m, Monad m) => Alternative (TrasaT m) where
  empty = lift empty
  a <|> b = catchError a (const b)

instance (Monad m, Alternative m) => MonadPlus (TrasaT m)

instance MonadTrans TrasaT where
  lift = TrasaT . lift . lift . lift

runTrasaT
  :: TrasaT m a
  -> M.Map (CI BS.ByteString) T.Text -- ^ Headers
  -> QueryString -- ^ Query string parameters
  -> BS.ByteString -- ^ Request body
  -> Maybe Wai.RequestBodyType -- ^ Request body type from @wai-extra@
  -> m (Either TrasaErr a, M.Map (CI BS.ByteString) T.Text)
runTrasaT trasa headers queryStrings requestBody mrequestBodyType = (flip runReaderT (TrasaEnv headers queryStrings requestBody mrequestBodyType) . flip runStateT M.empty . runExceptT  . unTrasaT) trasa

mapTrasaT :: (forall x. m x -> n x) -> TrasaT m a -> TrasaT n a
mapTrasaT eta = TrasaT . mapExceptT (mapStateT (mapReaderT eta)) . unTrasaT

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
  -> Wai.Application -- ^ Wai Application
serveWith toMeta makeResponse madeRouter =
  \req respond ->
    case decodeMethod <$> TE.decodeUtf8' (Wai.requestMethod req) of
      Left _ -> respond (Wai.responseLBS N.status400 [] "Non utf8 encoded request method")
      Right method -> case parseHeaders req of
        Left _ -> respond (Wai.responseLBS N.status400 [] "Non utf8 encoded headers")
        Right headers -> case parseAccepts headers of
          Nothing -> respond (Wai.responseLBS N.status415 [] "Accept header missing or malformed")
          Just accepts -> do
            content <- for (M.lookup hContentType headers >>= N.parseAccept . TE.encodeUtf8) $ \typ ->
              Content typ <$> Wai.strictRequestBody req
            (newReq, requestBody) <- getRequestBody req
            let queryStrings = decodeQuery (Wai.queryString newReq)
                url = Url (Wai.pathInfo newReq) queryStrings
                dispatch = dispatchWith toMeta makeResponse madeRouter method accepts url content
                mrequestBodyType = Wai.getRequestBodyType newReq
            runTrasaT dispatch headers queryStrings requestBody mrequestBodyType >>= \case
              (resErr,newHeaders) -> case join resErr of
                Left (TrasaErr stat errBody) ->
                  respond (Wai.responseLBS stat (encodeHeaders newHeaders) errBody)
                Right (Content typ lbs) -> do
                  let cType = TE.decodeUtf8 (N.renderHeader typ)
                      encodedHeaders = encodeHeaders (M.insert hContentType cType newHeaders)
                  respond (Wai.responseLBS N.status200 encodedHeaders lbs)
  where
    encodeHeaders = M.toList . fmap TE.encodeUtf8
    parseHeaders = traverse TE.decodeUtf8' . M.fromList . Wai.requestHeaders
    parseAccepts :: Headers
                 -> Maybe [N.MediaType]
    parseAccepts headers = case M.lookup hAccept headers of
      Nothing -> Just ["*/*"]
      Just accept -> (traverse N.parseAccept . fmap (TE.encodeUtf8 . T.dropAround (' '==)) . T.splitOn ",") accept

-- Inspecting the request body will cause its chunks to free:
-- so, we copoy the request to make it available.
-- Unfortunately the record selector is deprecated
getRequestBody :: Wai.Request -> IO (Wai.Request, BS.ByteString)
getRequestBody req = do
  body' <- loop id
  ichunks <- newIORef body'
  let rbody = atomicModifyIORef ichunks $ \chunks ->
         case chunks of
             [] -> ([], BS.empty)
             x:y -> (y, x)
  let req' = req { Wai.requestBody = rbody }
  pure (req', BS.concat body')
  where 
  loop front = do
    bs <- Wai.getRequestBodyChunk req
    if BS.null bs
    then pure $ front []
    else loop $ front . (bs:)
