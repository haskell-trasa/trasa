{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Trasa.Reflex
  ( requestWith
  , requestManyWith
  , ResponseHandler(..)
  , requestMultiWith
  , serve
  , Arguments
  , handler) where

import Data.Kind (Type)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS (toStrict,fromStrict)

import Data.Functor.Identity (Identity(..))
import Data.Vinyl (Rec(..))
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Types.Status as N
import Reflex.Dom

import Trasa.Core hiding (requestWith,Arguments,handler)

import Reflex.PopState

type family Arguments (caps :: [Type]) (resp :: Type) (result :: Type) :: Type where
  Arguments '[] resp result = resp -> result
  Arguments (cap:caps) resp result = cap -> Arguments caps resp result

handler :: Rec Identity caps -> ResponseBody Identity resp -> Arguments caps resp x -> x
handler = go
  where go :: Rec Identity caps -> ResponseBody Identity resp -> Arguments caps resp x -> x
        go RNil (ResponseBody (Identity response)) f = f response
        go (Identity cap :& caps) respBody f = go caps respBody (f cap)

-- | Not exported. Used internally so that requestManyInternal can be written
--   and used to implement both serve and requestMany.
data ResponseHandler route a = forall resp. ResponseHandler
  !(Prepared route resp)
  !(ResponseBody (Many BodyDecoding) resp)
  !(resp -> a)

data Pair a b = Pair !a !b
  deriving (Functor, Foldable, Traversable)

newtype Preps route resp f a = Preps (f (Pair (ResponseHandler route resp) a))
  deriving (Functor,Foldable,Traversable)

requestWith :: forall t m route response.
  MonadWidget t m
  => (forall caps req resp. route caps req resp -> T.Text)
  -> (forall caps req resp. route caps req resp -> Path CaptureEncoding caps)
  -> (forall caps req resp. route caps req resp -> RequestBody (Many BodyEncoding) req)
  -> (forall caps req resp. route caps req resp -> ResponseBody (Many BodyDecoding) resp)
  -> Event t (Prepared route response)
  -> m (Event t (Either TrasaErr response))
requestWith toMethod toCapEncs toReqBody toRespBody prepared =
  coerceEvent <$> requestManyWith toMethod toCapEncs toReqBody toRespBody preparedId
  where preparedId = coerceEvent prepared :: Event t (Identity (Prepared route response))

requestManyWith :: forall t m f route response.
  (MonadWidget t m, Traversable f)
  => (forall caps req resp. route caps req resp -> T.Text)
  -> (forall caps req resp. route caps req resp -> Path CaptureEncoding caps)
  -> (forall caps req resp. route caps req resp -> RequestBody (Many BodyEncoding) req)
  -> (forall caps req resp. route caps req resp -> ResponseBody (Many BodyDecoding) resp)
  -> Event t (f (Prepared route response))
  -> m (Event t (f (Either TrasaErr response)))
requestManyWith toMethod toCapEncs toReqBody toRespBody prepared =
  requestMultiWith toMethod toCapEncs toReqBody toRespBody (fmap toResponseHandler <$> prepared)
  where toResponseHandler p@(Prepared route _ _) = ResponseHandler p (toRespBody route) id

-- TODO: Are these error codes correct
requestMultiWith :: forall t m f route a.
  (MonadWidget t m, Traversable f)
  => (forall caps req resp. route caps req resp -> T.Text)
  -> (forall caps req resp. route caps req resp -> Path CaptureEncoding caps)
  -> (forall caps req resp. route caps req resp -> RequestBody (Many BodyEncoding) req)
  -> (forall caps req resp. route caps req resp -> ResponseBody (Many BodyDecoding) resp)
  -> Event t (f (ResponseHandler route a))
  -> m (Event t (f (Either TrasaErr a)))
requestMultiWith toMethod toCapEncs toReqBody toRespBody contResp =
  fmap parseXhrResponses <$> performRequestsAsync (buildXhrRequests <$> contResp)
  where parseXhrResponses :: Preps route a f XhrResponse -> f (Either TrasaErr a)
        parseXhrResponses (Preps res) = fmap parseOneXhrResponse res
        parseOneXhrResponse :: Pair (ResponseHandler route a) XhrResponse -> Either TrasaErr a
        parseOneXhrResponse (Pair (ResponseHandler (Prepared route _ _) _ fromResp)
                            (XhrResponse statusCode _ _ response headers)) =
          case statusCode < 400 of
            True -> case M.lookup "Content-Type" headers of
              Just content -> case response of
                Just txt -> let bs = LBS.fromStrict (TE.encodeUtf8 txt) in
                  case decodeResponseBody (toRespBody route) (Content content bs) of
                    Just a -> Right (fromResp a)
                    Nothing -> Left (TrasaErr N.status400 "Could not decode resp body")
                Nothing -> Left (TrasaErr N.status400 "No body returned from server")
              Nothing -> Left (TrasaErr N.status406 "No content type from server")
            False -> Left (TrasaErr (N.mkStatus (fromIntegral statusCode) (maybe "" TE.encodeUtf8 response)) "")
        buildXhrRequests :: f (ResponseHandler route a) -> Preps route a f (XhrRequest BS.ByteString)
        buildXhrRequests = Preps . fmap buildOneXhrRequest
        buildOneXhrRequest :: ResponseHandler route a -> Pair (ResponseHandler route a) (XhrRequest BS.ByteString)
        buildOneXhrRequest w@(ResponseHandler p@(Prepared route _ _) _ _) =
          Pair w (XhrRequest (toMethod route) (encodeUrl (linkWith toCapEncs p)) conf)
          where conf :: XhrRequestConfig BS.ByteString
                conf = def & xhrRequestConfig_sendData .~ maybe "" (LBS.toStrict . contentData) content
                           & xhrRequestConfig_headers .~ headers
                           & xhrRequestConfig_responseHeaders .~ AllHeaders
                headers = maybe acceptHeader (\ct -> M.insert "Content-Type" (contentType ct) acceptHeader) content
                acceptHeader = "Accept" =: T.intercalate ", " (toList accepts)
                Payload _ content accepts = payloadWith toCapEncs toReqBody toRespBody p

serve :: forall t m route.
  MonadWidget t m
  => (forall caps req resp. route caps req resp -> T.Text)
  -> (forall caps req resp. route caps req resp -> Path CaptureCodec caps)
  -> (forall caps req resp. route caps req resp -> RequestBody (Many BodyCodec) req)
  -> (forall caps req resp. route caps req resp -> ResponseBody (Many BodyCodec) resp)
  -> Router route
  -> (forall caps req resp.
      route caps req resp ->
      Rec Identity caps ->
      ResponseBody Identity resp ->
      m (Event t (Concealed route)))
  -> (TrasaErr -> m (Event t (Concealed route)))
  -> m ()
serve toMethod toCapCodec toReqBody toRespBody router widgetize onErr = mdo
  -- Investigate why this is needed
  let newUrls = ffor (switch (current jumpsD)) $ \(Concealed route caps reqBody) ->
        linkWith toCapEnc (Prepared route caps reqBody)
  (u0, urls) <- url newUrls
  pb <- getPostBuild
  let choice = ffor (leftmost [newUrls, urls, u0 <$ pb]) $ \us ->
        parseWith toReqBodyDec router "GET" us Nothing
      (failures, concealeds) = fanEither choice
  actions <- requestMultiWith toMethod toCapEnc toReqBodyEnc toRespBodyDec (fromConcealed <$> concealeds)
  jumpsD <- widgetHold (return never) (leftmost [onErr <$> failures, either onErr id . runIdentity <$> actions])
  return ()
  where toCapEnc :: route caps req resp -> Path CaptureEncoding caps
        toCapEnc = mapPath captureCodecToCaptureEncoding . toCapCodec
        toReqBodyDec :: route caps req resp -> RequestBody (Many BodyDecoding) req
        toReqBodyDec = mapRequestBody (mapMany bodyCodecToBodyDecoding) . toReqBody
        toReqBodyEnc :: route caps req resp -> RequestBody (Many BodyEncoding) req
        toReqBodyEnc = mapRequestBody (mapMany bodyCodecToBodyEncoding) . toReqBody
        toRespBodyDec :: route caps req resp -> ResponseBody (Many BodyDecoding) resp
        toRespBodyDec = mapResponseBody (mapMany bodyCodecToBodyDecoding) . toRespBody
        fromConcealed :: Concealed route -> Identity (ResponseHandler route (m (Event t (Concealed route))))
        fromConcealed (Concealed route caps reqBody) =
          Identity (ResponseHandler (Prepared route caps reqBody) (toRespBodyDec route)
                   (widgetize route caps . ResponseBody . Identity))
