{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Trasa.Reflex where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS (toStrict,fromStrict)
import qualified Data.Binary.Builder as B (toLazyByteString)

import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Network.HTTP.Types.URI
import Reflex.Dom

import Trasa.Core

request :: forall t m rt rp.
  MonadWidget t m
  => (forall cs' rq' rp'. rt cs' rq' rp' -> T.Text)
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureEncoding cs')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyEncoding) rq')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody (Many BodyDecoding) rp')
  -> Event t (Prepared rt rp)
  -> m (Event t (Either T.Text rp))
request toMethod toCapEncs toReqBody toRespBody prepared =
  fmap parseXhrResponse <$> performRequestsAsync (xhrRequest <$> prepared)
  where parseXhrResponse :: (Prepared rt rp, XhrResponse) -> Either T.Text rp
        parseXhrResponse (Prepared route _ _, res) = case M.lookup "Content-Type" (_xhrResponse_headers res) of
          Just content -> case _xhrResponse_responseText res of
            Just text -> let bs = LBS.fromStrict (TE.encodeUtf8 text) in
              case decodeResponseBody (toRespBody route) (Content content bs) of
                Just a -> Right a
                Nothing -> Left "Could not decode body"
            Nothing -> Left "No body returned from server"
          Nothing -> Left "No content type from server"
        xhrRequest :: Prepared rt rp -> (Prepared rt rp, XhrRequest BS.ByteString)
        xhrRequest p@(Prepared route captures reqBody) =
          (p, XhrRequest (toMethod route) (url p) conf)
          where url :: Prepared rt rp -> T.Text
                -- This use of decodeUtf8 is safe because http-types has a postconditon
                -- that the builder is utf8 encoded
                url = TE.decodeUtf8 . LBS.toStrict . B.toLazyByteString . encodePathSegments . linkWith toCapEncs
                conf :: XhrRequestConfig BS.ByteString
                conf = def & xhrRequestConfig_sendData .~ maybe "" (LBS.toStrict . contentData) content
                           & xhrRequestConfig_headers .~ headers
                headers = maybe acceptHeader (\ct -> M.insert "Content-Type" (contentType ct) acceptHeader) content
                acceptHeader = "Accept" =: T.intercalate ", " (toList accepts)
                Payload pathSegments content accepts = payloadWith toCapEncs toReqBody toRespBody p
