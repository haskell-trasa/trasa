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
module Trasa.Reflex (requestWith,requestManyWith,serve,Arguments,handler) where

import Data.Kind (Type)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS (toStrict,fromStrict)

import Data.Functor.Identity (Identity(..))
import Data.Vinyl (Rec(..))
import Data.Foldable (toList)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Types.Status as N
import Reflex.Dom

import Trasa.Core hiding (requestWith,Arguments,handler)

import Reflex.PopState

type family Arguments (captures :: [Type]) (response :: Type) (result :: Type) :: Type where
  Arguments '[] response result = response -> result
  Arguments (cap:caps) response result = cap -> Arguments caps response result

handler :: Rec Identity captures -> ResponseBody Identity response -> Arguments captures response x -> x
handler = go
  where go :: Rec Identity caps -> ResponseBody Identity response -> Arguments caps response x -> x
        go RNil (ResponseBody (Identity response)) f = f response
        go (Identity cap :& caps) responseBody f = go caps responseBody (f cap)

-- | Not exported. Used internally so that requestManyInternal can be written
--   and used to implement both serve and requestMany.
data WithResp route a = forall response. WithResp
  !(Prepared route response)
  !(ResponseBody (Many BodyDecoding) response)
  !(response -> a)

data Pair a b = Pair !a !b
  deriving (Functor, Foldable, Traversable)

newtype Preps route response f a = Preps (f (Pair (WithResp route response) a))
  deriving (Functor,Foldable,Traversable)

withResp ::
   (forall captures request response. route captures request response -> ResponseBody (Many BodyDecoding) response)
   -> Prepared route resp
   -> WithResp route resp
withResp toReqBody p@(Prepared route _ _) = WithResp p (toReqBody route) id

requestWith :: forall t m route resp.
  MonadWidget t m
  => (forall captures request response. route captures request response -> T.Text)
  -> (forall captures request response. route captures request response -> Path CaptureEncoding captures)
  -> (forall captures request response. route captures request response -> RequestBody (Many BodyEncoding) request)
  -> (forall captures request response. route captures request response -> ResponseBody (Many BodyDecoding) response)
  -> Event t (Prepared route resp)
  -> m (Event t (Either TrasaErr resp))
requestWith toMethod toCapEncs toReqBody toRespBody prepared =
  coerceEvent <$> requestManyWith toMethod toCapEncs toReqBody toRespBody preparedId
  where preparedId = coerceEvent prepared :: Event t (Identity (Prepared route resp))

requestManyWith :: forall t m f route resp.
  (MonadWidget t m, Traversable f)
  => (forall captures request response. route captures request response -> T.Text)
  -> (forall captures request response. route captures request response -> Path CaptureEncoding captures)
  -> (forall captures request response. route captures request response -> RequestBody (Many BodyEncoding) request)
  -> (forall captures request response. route captures request response -> ResponseBody (Many BodyDecoding) response)
  -> Event t (f (Prepared route resp))
  -> m (Event t (f (Either TrasaErr resp)))
requestManyWith toMethod toCapEncs toReqBody toRespBody prepared =
  requestManyInternal toMethod toCapEncs toReqBody toRespBody (fmap (withResp toRespBody) <$> prepared)

-- TODO: Are these error codes correct
requestManyInternal :: forall t m f route a.
  (MonadWidget t m, Traversable f)
  => (forall captures request response. route captures request response -> T.Text)
  -> (forall captures request response. route captures request response -> Path CaptureEncoding captures)
  -> (forall captures request response. route captures request response -> RequestBody (Many BodyEncoding) request)
  -> (forall captures request response. route captures request response -> ResponseBody (Many BodyDecoding) response)
  -> Event t (f (WithResp route a))
  -> m (Event t (f (Either TrasaErr a)))
requestManyInternal toMethod toCapEncs toReqBody toRespBody contResp =
  fmap parseXhrResponses <$> performRequestsAsync (buildXhrRequests <$> contResp)
  where parseXhrResponses :: Preps route a f XhrResponse -> f (Either TrasaErr a)
        parseXhrResponses (Preps res) = fmap parseOneXhrResponse res
        parseOneXhrResponse :: Pair (WithResp route a) XhrResponse -> Either TrasaErr a
        parseOneXhrResponse (Pair (WithResp (Prepared route _ _) _ fromResp) xhrRes) =
          case M.lookup "Content-Type" (_xhrResponse_headers xhrRes) of
            Just content -> case _xhrResponse_responseText xhrRes of
              Just txt -> let bs = LBS.fromStrict (TE.encodeUtf8 txt) in
                case decodeResponseBody (toRespBody route) (Content content bs) of
                  Just a -> Right (fromResp a)
                  Nothing -> Left (TrasaErr N.status400 "Could not decode response body")
              Nothing -> Left (TrasaErr N.status400 "No body returned from server")
            Nothing -> Left (TrasaErr N.status406 "No content type from server")
        buildXhrRequests :: f (WithResp route a) -> Preps route a f (XhrRequest BS.ByteString)
        buildXhrRequests = Preps . fmap buildOneXhrRequest
        buildOneXhrRequest :: WithResp route a -> Pair (WithResp route a) (XhrRequest BS.ByteString)
        buildOneXhrRequest w@(WithResp p@(Prepared route _ _) _ _) =
          Pair w (XhrRequest (toMethod route) (encodeUrl (linkWith toCapEncs p)) conf)
          where conf :: XhrRequestConfig BS.ByteString
                conf = def & xhrRequestConfig_sendData .~ maybe "" (LBS.toStrict . contentData) content
                           & xhrRequestConfig_headers .~ headers
                           & xhrRequestConfig_responseHeaders .~ OnlyHeaders (S.singleton "Content-Type")
                headers = maybe acceptHeader (\ct -> M.insert "Content-Type" (contentType ct) acceptHeader) content
                acceptHeader = "Accept" =: T.intercalate ", " (toList accepts)
                Payload _ content accepts = payloadWith toCapEncs toReqBody toRespBody p

serve :: forall t m route.
  MonadWidget t m
  => (forall captures request response. route captures request response -> T.Text)
  -> (forall captures request response. route captures request response -> Path CaptureCodec captures)
  -> (forall captures request response. route captures request response -> RequestBody (Many BodyCodec) request)
  -> (forall captures request response. route captures request response -> ResponseBody (Many BodyCodec) response)
  -> Router route
  -> (forall captures request response.
      route captures request response ->
      Rec Identity captures ->
      ResponseBody Identity response ->
      m (Event t (Concealed route)))
  -> (TrasaErr -> m (Event t (Concealed route)))
  -> m ()
serve toMethod toCapCodec toReqBody toRespBody router widgetize onErr = mdo
  (u0, urls) <- url $ ffor (switch (current jumpsD)) $ \(Concealed route captures reqBody) ->
    linkWith toCapEnc (Prepared route captures reqBody)
  pb <- getPostBuild
  let choice = ffor (leftmost [urls, u0 <$ pb]) $ \us ->
        parseWith toReqBodyDec router "GET" us Nothing
      (failures, concealeds) = fanEither choice
  actions <- requestManyInternal toMethod toCapEnc toReqBodyEnc toRespBodyDec (fromConcealed <$> concealeds)
  jumpsD <- widgetHold (return never) (leftmost [onErr <$> failures, either onErr id . runIdentity <$> actions])
  return ()
  where toCapEnc :: route captures request response -> Path CaptureEncoding captures
        toCapEnc = mapPath captureCodecToCaptureEncoding . toCapCodec
        toReqBodyDec :: route captures request response -> RequestBody (Many BodyDecoding) request
        toReqBodyDec = mapRequestBody (mapMany bodyCodecToBodyDecoding) . toReqBody
        toReqBodyEnc :: route captures request response -> RequestBody (Many BodyEncoding) request
        toReqBodyEnc = mapRequestBody (mapMany bodyCodecToBodyEncoding) . toReqBody
        toRespBodyDec :: route captures request response -> ResponseBody (Many BodyDecoding) response
        toRespBodyDec = mapResponseBody (mapMany bodyCodecToBodyDecoding) . toRespBody
        fromConcealed :: Concealed route -> Identity (WithResp route (m (Event t (Concealed route))))
        fromConcealed (Concealed route caps reqBody) =
          Identity (WithResp (Prepared route caps reqBody) (toRespBodyDec route)
                   (widgetize route caps . ResponseBody . Identity))
