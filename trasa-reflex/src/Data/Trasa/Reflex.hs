{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Trasa.Reflex where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Binary.Builder as B (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS (toStrict)

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
  fmap parseXhrResponse <$> performRequestAsync (xhrRequest <$> prepared)
  where parseXhrResponse :: XhrResponse -> Either T.Text rp
        parseXhrResponse = undefined
        xhrRequest :: Prepared rt rp -> XhrRequest ()
        xhrRequest p@(Prepared route captures reqBody) =
          XhrRequest (toMethod route) (url p) conf
          where url :: Prepared rt rp -> T.Text
                -- This use of decodeUtf8 is safe because http-types has a postconditon that the builder is utf8 encoded
                url = TE.decodeUtf8 . LBS.toStrict . B.toLazyByteString . encodePathSegments . linkWith toCapEncs
                conf :: XhrRequestConfig ()
                conf = def & xhrRequestConfig_sendData .~ ()
