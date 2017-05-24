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
  ( TrasaT(..)
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
import Control.Monad.Except (ExceptT,runExceptT,MonadError(..),MonadIO(..))
import Control.Monad.State.Strict (StateT,runStateT,MonadState(..))

import Trasa.Core

newtype TrasaT a = TrasaT
  { unTrasaT :: ExceptT TrasaErr (StateT (M.Map (CI BS.ByteString) T.Text) IO) a
  } deriving (Functor,Applicative,Monad,MonadError TrasaErr,MonadState (M.Map (CI BS.ByteString) T.Text),MonadIO)

runTrasaT
  :: TrasaT a
  -> M.Map (CI BS.ByteString) T.Text -- ^ Headers
  -> IO (Either TrasaErr a, M.Map (CI BS.ByteString) T.Text)
runTrasaT trasa headers = (flip runStateT headers . runExceptT  . unTrasaT) trasa

serve ::
     (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyDecoding) rq')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody (Many BodyEncoding) rp')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Rec Identity cs' -> RequestBody Identity rq' -> TrasaT rp')
  -> Router rt -- ^ Router
  -> WAI.Application -- ^ WAI Application
serve toReqBody toRespBody makeResponse router =
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
            let path = WAI.pathInfo req
                dispatch = dispatchWith toReqBody toRespBody makeResponse router method accepts path content
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
