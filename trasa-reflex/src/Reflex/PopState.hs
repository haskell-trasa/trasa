{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Reflex.PopState (url,decodeUrl,encodeUrl) where

import qualified Data.Text as T
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
import Trasa.Core (Url,decodeUrl,encodeUrl)

getPopState :: (Reflex t, TriggerEvent t m, MonadJSM m) => m (Event t Url)
getPopState = do
  window <- currentWindowUnchecked
  wrapDomEvent window (`on` popState) $ do
    loc <- getLocation window
    locStr <- getPathname loc
    (return . decodeUrl) locStr

url :: (MonadHold t m, TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m), MonadJSM m) =>
  Event t Url -> m (Url, Event t Url)
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
