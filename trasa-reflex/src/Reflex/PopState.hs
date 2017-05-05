{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Reflex.PopState (url,decodeUrl,encodeUrl) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Builder as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.URI (extractPath,decodePathSegments,encodePathSegments)
import Reflex.Class (Reflex(..),MonadHold(..),ffor)
import Reflex.PerformEvent.Class (PerformEvent(..))
import Reflex.TriggerEvent.Class (TriggerEvent)
import Reflex.Dom.Builder.Immediate (wrapDomEvent)
import Language.Javascript.JSaddle (eval,call)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Types (MonadJSM,liftJSM,ToJSVal(..))
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Window (getLocation)
import GHCJS.DOM.WindowEventHandlers (popState)
import GHCJS.DOM.Location (getPathname)

-- Move these into trasa core?
decodeUrl :: T.Text -> [T.Text]
decodeUrl = decodePathSegments . extractPath . T.encodeUtf8

-- This use of decodeUtf8 is safe because http-types.encodePathSegments
-- has a postconditon that the builder is utf8 encoded
encodeUrl :: [T.Text] -> T.Text
encodeUrl = T.decodeUtf8 . LBS.toStrict . LBS.toLazyByteString . encodePathSegments

getPopState :: (Reflex t, TriggerEvent t m, MonadJSM m) => m (Event t [T.Text])
getPopState = do
  window <- currentWindowUnchecked
  wrapDomEvent window (`on` popState) $ do
    loc <- getLocation window
    locStr <- getPathname loc
    (return . decodeUrl) locStr

url :: (MonadHold t m, TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m), MonadJSM m) =>
  Event t [T.Text] -> m ([T.Text], Event t [T.Text])
url us = do
  u0 <- liftJSM $ do
    window   <- currentWindowUnchecked
    loc <- getLocation window
    locStr   <- getPathname loc
    (return . decodeUrl) locStr
  performEvent_ $ ffor us $ \uri -> liftJSM $ do
    f <- eval ("(function (url) { window[\"history\"][\"pushState\"](0,\"\",url) })" :: T.Text)
    jsUri <- toJSVal (encodeUrl uri)
    _ <- call f f [jsUri]
    return ()
  ps <- getPopState
  return (u0, ps)
