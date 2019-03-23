{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Reflex.PopState (url) where

import Data.Monoid ((<>))
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Location (getPathname,getSearch)
import GHCJS.DOM.Types (MonadJSM,liftJSM,ToJSVal(..))
import GHCJS.DOM.Window (getLocation)
import GHCJS.DOM.WindowEventHandlers (popState)
import Language.Javascript.JSaddle (eval,call)
import Reflex.Class (Reflex(..),MonadHold(..),ffor)
import Reflex.Dom.Builder.Immediate (wrapDomEvent)
import Reflex.PerformEvent.Class (PerformEvent(..))
import Reflex.TriggerEvent.Class (TriggerEvent)
import Trasa.Core (Url,decodeUrl,encodeUrl)
import qualified Data.Text as T

getPopState :: (Reflex t, TriggerEvent t m, MonadJSM m) => m (Event t Url)
getPopState = do
  window <- currentWindowUnchecked
  wrapDomEvent window (`on` popState) $ do
    loc <- getLocation window
    locStr <- getPathname loc
    searchStr <- getSearch loc
    return (decodeUrl (locStr <> searchStr))

-- | The starting location and a stream of popstate urls
url :: (MonadHold t m, TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m), MonadJSM m) =>
  Event t Url -> m (Url, Event t Url)
url us = do
  u0 <- liftJSM $ do
    window   <- currentWindowUnchecked
    loc <- getLocation window
    locStr   <- getPathname loc
    searchStr <- getSearch loc
    return (decodeUrl (locStr <> searchStr))
  performEvent_ $ ffor us $ \uri -> liftJSM $ do
    f <- eval ("(function (url) { window[\"history\"][\"pushState\"](0,\"\",url) })" :: T.Text)
    jsUri <- toJSVal (encodeUrl uri)
    _ <- call f f [jsUri]
    return ()
  ps <- getPopState
  return (u0, ps)
