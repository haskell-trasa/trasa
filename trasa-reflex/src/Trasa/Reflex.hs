{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Trasa.Reflex (request,requestMany,serve) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS (toStrict,fromStrict)
import qualified Data.Binary.Builder as B (toLazyByteString)

import Data.Functor.Identity (Identity(..))
import Data.Vinyl (Rec(..))
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Network.HTTP.Types.URI
import Reflex.Dom

import Trasa.Core

import Reflex.PopState

data Preped rt rp a = Preped !(Prepared rt rp) !a
  deriving (Functor,Foldable,Traversable)

newtype Preps rt rp f a = Preps
  { getPreps :: f (Preped rt rp a) }
  deriving (Functor,Foldable,Traversable)

request :: forall t m rt rp.
  MonadWidget t m
  => (forall cs' rq' rp'. rt cs' rq' rp' -> T.Text)
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureEncoding cs')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyEncoding) rq')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody (Many BodyDecoding) rp')
  -> Event t (Prepared rt rp)
  -> m (Event t (Either TrasaErr rp))
request toMethod toCapEncs toReqBody toRespBody prepared =
  coerceEvent <$> requestMany toMethod toCapEncs toReqBody toRespBody preparedId
  where preparedId = coerceEvent prepared :: Event t (Identity (Prepared rt rp))

requestMany :: forall t m f rt rp.
  (MonadWidget t m, Traversable f)
  => (forall cs' rq' rp'. rt cs' rq' rp' -> T.Text)
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureEncoding cs')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyEncoding) rq')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody (Many BodyDecoding) rp')
  -> Event t (f (Prepared rt rp))
  -> m (Event t (f (Either TrasaErr rp)))
requestMany toMethod toCapEncs toReqBody toRespBody prepared =
  fmap parseXhrResponse <$> performRequestsAsync (buildXhrRequest <$> prepared)
  where parseXhrResponse :: Preps rt rp f XhrResponse -> f (Either TrasaErr rp)
        parseXhrResponse = undefined
        -- parseXhrResponse (Prepared route _ _, res) = case M.lookup "Content-Type" (_xhrResponse_headers res) of
        --   Just content -> case _xhrResponse_responseText res of
        --     Just txt -> let bs = LBS.fromStrict (TE.encodeUtf8 txt) in
        --       case decodeResponseBody (toRespBody route) (Content content bs) of
        --         Just a -> Right a
        --         Nothing -> Left "Could not decode body"
        --     Nothing -> Left "No body returned from server"
        --   Nothing -> Left "No content type from server"
        buildXhrRequest :: f (Prepared rt rp) -> Preps rt rp f (XhrRequest BS.ByteString)
        buildXhrRequest = undefined
        -- buildXhrRequest p@(Prepared route _ _) =
        --   (p, XhrRequest (toMethod route) (encodeUrl (linkWith toCapEncs p)) conf)
        --   where conf :: XhrRequestConfig BS.ByteString
        --         conf = def & xhrRequestConfig_sendData .~ maybe "" (LBS.toStrict . contentData) content
        --                    & xhrRequestConfig_headers .~ headers
        --         headers = maybe acceptHeader (\ct -> M.insert "Content-Type" (contentType ct) acceptHeader) content
        --         acceptHeader = "Accept" =: T.intercalate ", " (toList accepts)
        --         Payload _ content accepts = payloadWith toCapEncs toReqBody toRespBody p

serve :: forall t m rt.
  MonadWidget t m
  => (forall cs' rq' rp'. rt cs' rq' rp' -> T.Text)
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureCodec cs')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyCodec) rq')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody (Many BodyCodec) rp')
  -> [Constructed rt]
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Rec Identity cs' -> ResponseBody Identity rp' -> m ())
  -> (TrasaErr -> m ())
  -> m (Event t ())
serve toMethod toCapCodec toReqBody toRespBody enumeratedRoutes router onErr = do
  urls <- url never
  let choice = ffor (updated urls) $ \us ->
        parseWith toMethod toCapDec toReqBodyDec enumeratedRoutes "GET" us Nothing
  let (failures, concealeds) = fanEither choice
  actions <- fmap test <$> request toMethod toCapEnc toReqBodyEnc toRespBodyDec
    (ffor concealeds (\(Concealed route caps reqBody) -> Prepared route caps reqBody))
  updated <$> widgetHold (return ()) (leftmost [onErr <$> failures,actions])
  where toCapDec = mapPath captureCodecToCaptureDecoding . toCapCodec
        toCapEnc = mapPath captureCodecToCaptureEncoding . toCapCodec
        toReqBodyDec = mapRequestBody (mapMany bodyCodecToBodyDecoding) . toReqBody
        toReqBodyEnc = mapRequestBody (mapMany bodyCodecToBodyEncoding) . toReqBody
        toRespBodyDec = mapResponseBody (mapMany bodyCodecToBodyDecoding) . toRespBody
        test :: forall rp. Either TrasaErr rp -> m ()
        test = undefined
