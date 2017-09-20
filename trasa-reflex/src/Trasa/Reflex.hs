{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Trasa.Reflex
  ( MetaReflex
  , metaCodecToMetaReflex
  , metaReflexToMetaClient
  , requestWith
  , requestManyWith
  , ResponseHandler(..)
  , requestMultiWith
  , serveWith
  , Arguments
  , handler
  , Requiem(..)
  , serveDynamicWith
  , ResponseError(..)
  , DecodeError(..)
  ) where

import Data.Kind (Type)

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS (toStrict,fromStrict)

import Data.Bifunctor (first)
import Data.Monoid ((<>))
import Data.Type.Equality ((:~:)(Refl))
import Data.Functor.Identity (Identity(..))
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Media as N
import Reflex.Dom
import Control.Monad (forM,when)
import Data.Text (Text)
import Data.Map.Strict (Map)

import Trasa.Core hiding (requestWith,Arguments,handler)

import Reflex.PopState

data DecodeError
  = BadStatusCode !Word
  | MissingContentType -- Content-Type header was missing
  | UnparseableContentType !Text
  | MissingBody -- No response body
  | UnsupportedMediaType !N.MediaType -- Received a bad content-type from the server
  | UnparseableContent !Text -- The response body could not be parsed, field is error returned by parser

data ResponseError = ResponseError !XhrResponse !DecodeError

-- | Replaces 'Trasa.Core.Arguments' with one that does not deal with request bodies
type family Arguments (caps :: [Type]) (qrys :: [Param]) (resp :: Type) (result :: Type) :: Type where
  Arguments '[] '[] resp result = resp -> result
  Arguments '[] (q:qs) resp result = ParamBase q -> Arguments '[] qs resp result
  Arguments (cap:caps) qs resp result = cap -> Arguments caps qs resp result

-- | "Trasa.Reflex.handler" is to "Trasa.Core.handler" as "Trasa.Reflex.Arguments" is to "Trasa.Core.Arguments"
handler :: Rec Identity caps -> Rec Parameter qrys -> ResponseBody Identity resp -> Arguments caps qrys resp x -> x
handler = go
  where
    go :: Rec Identity caps -> Rec Parameter qrys -> ResponseBody Identity resp -> Arguments caps qrys resp x -> x
    go RNil RNil (ResponseBody (Identity response)) f = f response
    go RNil (q :& qs) respBody f = go RNil qs respBody (f (demoteParameter q))
    go (Identity cap :& caps) qs respBody f = go caps qs respBody (f cap)

-- | Used when you want to perform an action for any response type
data ResponseHandler route a = forall resp. ResponseHandler
  !(Prepared route resp)
  !(ResponseBody (Many BodyDecoding) resp)
  !(resp -> a)

data Pair a b = Pair !a !b
  deriving (Functor, Foldable, Traversable)

newtype Preps route resp f a = Preps (f (Pair (ResponseHandler route resp) a))
  deriving (Functor,Foldable,Traversable)

type MetaReflex = Meta CaptureEncoding CaptureCodec (Many BodyCodec) (Many BodyDecoding)

metaCodecToMetaReflex :: MetaCodec caps qrys req resp -> MetaReflex caps qrys req resp
metaCodecToMetaReflex = mapMeta captureEncoding id id (mapMany bodyDecoding)

metaReflexToMetaClient :: MetaReflex caps qrys req resp -> MetaClient caps qrys req resp
metaReflexToMetaClient = mapMeta id captureEncoding (mapMany bodyEncoding) id

-- | Single request version of 'requestManyWith'
requestWith
  :: forall t m route response
  .  MonadWidget t m
  => (forall caps qrys req resp. route caps qrys req resp -> MetaClient caps qrys req resp)
  -> Event t (Prepared route response)
  -> m (Event t (Either ResponseError response))
requestWith toMeta prepared =
  coerceEvent <$> requestManyWith toMeta preparedId
  where preparedId = coerceEvent prepared :: Event t (Identity (Prepared route response))

-- | Perform n requests and collect the results
requestManyWith
  :: forall t m f route response
  .  (MonadWidget t m, Traversable f)
  => (forall caps qrys req resp. route caps qrys req resp -> MetaClient caps qrys req resp)
  -> Event t (f (Prepared route response))
  -- ^ The routes to request
  -> m (Event t (f (Either ResponseError response)))
requestManyWith toMeta prepared =
  requestMultiWith toMeta (fmap toResponseHandler <$> prepared)
  where toResponseHandler p@(Prepared route _ _ _) = ResponseHandler p (metaResponseBody (toMeta route)) id

decodeResponseBody' :: ResponseBody (Many BodyDecoding) response -> Content -> Either DecodeError response
decodeResponseBody' (ResponseBody (Many decodings)) (Content name content) = go (toList decodings)
  where
    go :: [BodyDecoding response] -> Either DecodeError response
    go [] = Left (UnsupportedMediaType name)
    go (BodyDecoding names dec:decs) = case any (N.matches name) names of
      True -> first UnparseableContent (dec content)
      False -> go decs

-- | Internal function but exported because it subsumes the function of all the other functions in this package.
-- | Very powerful function
requestMultiWith :: forall t m f route a.
  (MonadWidget t m, Traversable f)
  => (forall caps qrys req resp. route caps qrys req resp -> MetaClient caps qrys req resp)
  -> Event t (f (ResponseHandler route a))
  -> m (Event t (f (Either ResponseError a)))
requestMultiWith toMeta contResp =
  fmap parseXhrResponses <$> performRequestsAsync (buildXhrRequests <$> contResp)
  where parseXhrResponses :: Preps route a f XhrResponse -> f (Either ResponseError a)
        parseXhrResponses (Preps res) = fmap parseOneXhrResponse res
        parseOneXhrResponse :: Pair (ResponseHandler route a) XhrResponse -> Either ResponseError a
        parseOneXhrResponse (Pair (ResponseHandler _ responseBody fromResp) xhrResp@(XhrResponse statusCode _ _ response headers)) = first (ResponseError xhrResp) $ do
          when (statusCode >= 400) (Left (BadStatusCode statusCode))
          theContentType <- maybe (Left MissingContentType) Right (M.lookup "Content-Type" headers)
          content <- maybe (Left (UnparseableContentType theContentType)) Right (N.parseAccept (TE.encodeUtf8 theContentType))
          txt <- maybe (Left MissingBody) Right response
          let bs = LBS.fromStrict (TE.encodeUtf8 txt)
          fmap fromResp (decodeResponseBody' responseBody (Content content bs))
        buildXhrRequests :: f (ResponseHandler route a) -> Preps route a f (XhrRequest BS.ByteString)
        buildXhrRequests = Preps . fmap buildOneXhrRequest
        buildOneXhrRequest :: ResponseHandler route a -> Pair (ResponseHandler route a) (XhrRequest BS.ByteString)
        buildOneXhrRequest w@(ResponseHandler p@(Prepared route _ _ _) _ _) =
          Pair w (XhrRequest method (encodeUrl (linkWith toMeta p)) conf)
          where
            method = (encodeMethod . metaMethod . toMeta) route
            conf :: XhrRequestConfig BS.ByteString
            conf = def
              & xhrRequestConfig_sendData .~ maybe "" (LBS.toStrict . contentData) content
              & xhrRequestConfig_headers .~ headers
              & xhrRequestConfig_responseHeaders .~ AllHeaders
            -- headers = maybe acceptHeader (\ct -> M.insert "Content-Type" (contentType ct) acceptHeader) content
            headers = case content of
              Nothing -> acceptHeader
              Just (Content typ _) -> M.insert "Content-Type" (TE.decodeUtf8 (N.renderHeader typ)) acceptHeader
            acceptHeader = "Accept" =: (TE.decodeUtf8 . BS.intercalate ", " . fmap N.renderHeader . toList) accepts
            Payload _ content accepts = payloadWith toMeta p

-- | Used to serve single page apps
serveWith
  :: forall t m route
  .  MonadWidget t m
  => (forall caps qrys req resp. route caps qrys req resp -> MetaReflex caps qrys req resp)
  -> Router route
  -> (forall caps qrys req resp.
      route caps qrys req resp ->
      Rec Identity caps ->
      Rec Parameter qrys ->
      ResponseBody Identity resp ->
      m (Event t (Concealed route)))
  -- ^ Build a widget from captures, query parameters, and a response body
  -> (ResponseError -> m (Event t (Concealed route)))
  -> m ()
serveWith toMeta madeRouter widgetize onErr = mdo
  -- Investigate why this is needed
  let newUrls :: Event t Url
      newUrls = ffor (switch (current jumpsD)) $ \(Concealed route caps querys reqBody) ->
        linkWith (mapMetaQuery captureEncoding . toMeta) (Prepared route caps querys reqBody)
  (u0, urls) <- url newUrls
  pb <- getPostBuild
  let transMetaParse = mapMetaQuery captureDecoding . mapMetaRequestBody (mapMany bodyDecoding)
      choice :: Event t (Either TrasaErr (Concealed route))
      choice = ffor (leftmost [newUrls, urls, u0 <$ pb]) $ \us ->
        parseWith (transMetaParse . toMeta) madeRouter "GET" us Nothing
      (_failures, concealeds) = fanEither choice
  actions <- requestMultiWith (metaReflexToMetaClient . toMeta) (fromConcealed <$> concealeds)
  jumpsD <- widgetHold (return never) (leftmost [either onErr id . runIdentity <$> actions])
  return ()
  where
    fromConcealed :: Concealed route -> Identity (ResponseHandler route (m (Event t (Concealed route))))
    fromConcealed (Concealed route caps querys reqBody) =
      Identity (ResponseHandler (Prepared route caps querys reqBody) (metaResponseBody (toMeta route))
               (widgetize route caps querys . ResponseBody . Identity))

data Requiem caps qrys resp = Requiem
  { requiemCaptures :: Rec Identity caps
  , requiemQueries :: Rec Parameter qrys
  , requiemResponse :: ResponseBody Identity resp
  }

serveDynamicWith :: forall t m (route :: [Type] -> [Param] -> Bodiedness -> Type -> Type). MonadWidget t m
  => (forall caps1 qrys1 req1 resp1 caps2 qrys2 req2 resp2. route caps1 qrys1 req1 resp1 -> route caps2 qrys2 req2 resp2 -> Maybe ('(caps1,qrys1,req1,resp1) :~: '(caps2,qrys2,req2,resp2)))
  -> (forall caps qrys req resp. route caps qrys req resp -> MetaReflex caps qrys req resp)
  -> Router route
  -> (forall caps qrys req resp.
         route caps qrys req resp
      -> Event t (Requiem caps qrys resp)
      -> (Map Text Text, m (Event t (Concealed route))) -- first item is css class
     )
  -- ^ Build a widget from captures, query parameters, and a response body
  -> (Event t ResponseError -> (Map Text Text, m (Event t (Concealed route)))) -- first item is css class
  -> [Constructed route]
  -> Event t (Concealed route) -- ^ extra jumps, possibly from menu bar
  -> m (Event t (Concealed route))
serveDynamicWith testRouteEquality toMeta madeRouter widgetize onErr routes extraJumps = mdo
  -- Investigate why this is needed
  let newUrls :: Event t Url
      newUrls = ffor (leftmost [jumpsE,errJumpsE,extraJumps]) $ \(Concealed route caps querys reqBody) ->
        linkWith (mapMetaQuery captureEncoding . toMeta) (Prepared route caps querys reqBody)
  (u0, urls) <- url newUrls
  pb <- getPostBuild
  let transMetaParse = mapMetaQuery captureDecoding . mapMetaRequestBody (mapMany bodyDecoding)
      choice :: Event t (Either TrasaErr (Concealed route))
      choice = ffor (leftmost [newUrls, urls, u0 <$ pb]) $ \us ->
        parseWith (transMetaParse . toMeta) madeRouter "GET" us Nothing
      -- currently ignoring parse failures
      (_parseFailures, concealeds) = fanEither choice
  actions' :: Event t (Identity (Either ResponseError (FullMonty route))) <- requestMultiWith (metaReflexToMetaClient . toMeta) (fromConcealed <$> concealeds)
  let actions = (coerceEvent actions' :: Event t (Either ResponseError (FullMonty route)))
  let hidden = M.singleton "style" "display:none;"
  jumpsE <- fmap leftmost $ forM routes $ \(Constructed route) -> do
    let m = fmap (either (const Nothing) (castRequiem route)) actions
    attrs <- holdDyn hidden (fmap (maybe hidden (const M.empty)) m)
    let (rtStaticAttrs,rtWidg) = widgetize route (fmapMaybe id m)
    elDynAttr "div" (fmap (M.unionWith (<>) rtStaticAttrs) attrs) $ do
      rtWidg
  let merr = fmap (either Just (const Nothing)) actions
  errAttrs <- holdDyn hidden (fmap (maybe hidden (const M.empty)) merr)
  let (errStaticAttrs,errWidg) = onErr (fmapMaybe id merr)
  errJumpsE <- elDynAttr "div" (fmap (M.unionWith (<>) errStaticAttrs) errAttrs) $ do
    errWidg
  return concealeds
  where
  castRequiem :: route w x y z -> FullMonty route -> Maybe (Requiem w x z)
  castRequiem route (FullMonty incomingRoute caps querys theResp) = case testRouteEquality route incomingRoute of
    Nothing -> Nothing
    Just Refl -> Just (Requiem caps querys theResp)
  fromConcealed :: Concealed route -> Identity (ResponseHandler route (FullMonty route))
  fromConcealed (Concealed route caps querys reqBody) = Identity
    ( ResponseHandler
      (Prepared route caps querys reqBody)
      (metaResponseBody (toMeta route))
      (FullMonty route caps querys . ResponseBody . Identity)
    )

data FullMonty :: ([Type] -> [Param] -> Bodiedness -> Type -> Type) -> Type where
  FullMonty ::
       !(route captures querys request response)
    -> !(Rec Identity captures)
    -> !(Rec Parameter querys)
    -> !(ResponseBody Identity response)
    -> FullMonty route
  

