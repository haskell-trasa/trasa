{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Trasa.Client where

import Data.Semigroup ((<>))
import Data.Word (Word16)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary.Builder as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT hiding (singleton)
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.Builder.Int as LT
import qualified Network.HTTP.Types.URI as N
import qualified Network.HTTP.Types.Header as N
import qualified Network.HTTP.Types.Status as N
import qualified Network.HTTP.Client as N

import Data.Vinyl (Rec)
import Trasa.Core

data Scheme = Http | Https

schemeToSecure :: Scheme -> Bool
schemeToSecure = \case
  Http -> False
  Https -> True

schemeToPort :: Scheme -> Int
schemeToPort = \case
  Http -> 80
  Https -> 443

data Authority = Authority
  { authorityScheme :: !Scheme
  , authorityHost :: !T.Text
  , authorityPort :: Maybe Word16
  }

encodeAuthority :: T.Text -> Maybe Word16 -> BS.ByteString
encodeAuthority host port =
  (TE.encodeUtf8 . LT.toStrict . LT.toLazyText)
  (LT.fromText host <> maybe "" (\p -> LT.singleton ':' <> LT.decimal p) port)

encodePathBS :: [T.Text] -> BS.ByteString
encodePathBS = LBS.toStrict . LBS.toLazyByteString . (LBS.putCharUtf8 '/' <>) . N.encodePathSegmentsRelative

encodeQueryBS :: QueryString -> BS.ByteString
encodeQueryBS =
  LBS.toStrict .
  LBS.toLazyByteString .
  N.renderQueryBuilder True .
  encodeQuery

encodeAcceptBS :: [T.Text] -> BS.ByteString
encodeAcceptBS = TE.encodeUtf8 . T.intercalate ", "

data Config = Config
  { configAuthority :: Authority
  , configManager :: !(Maybe N.Manager)
  }

clientWith :: forall route response.
     (forall caps qrys req resp. route caps qrys req resp -> T.Text)
  -> (forall caps qrys req resp. route caps qrys req resp -> Path CaptureEncoding caps)
  -> (forall caps qrys req resp. route caps qrys req resp -> Rec (Query CaptureEncoding) qrys)
  -> (forall caps qrys req resp. route caps qrys req resp -> RequestBody (Many BodyEncoding) req)
  -> (forall caps qrys req resp. route caps qrys req resp -> ResponseBody (Many BodyDecoding) resp)
  -> Config
  -> Prepared route response
  -> IO (Either TrasaErr response)
clientWith toMethod toCapEnc toQuerys toReqBody toRespBody config =
  requestWith toMethod toCapEnc toQuerys toReqBody toRespBody run
  where
    run :: T.Text -> Url -> Maybe Content -> [T.Text] -> IO (Either TrasaErr Content)
    run method (Url path query) mcontent accepts  = do
      manager <- case mmanager of
        Nothing -> N.newManager N.defaultManagerSettings
        Just manager -> return manager
      response <- N.httpLbs req manager
      return $ case lookup N.hContentType (N.responseHeaders response) of
        Nothing -> Left (TrasaErr N.status415 "No content type found")
        Just bs -> case TE.decodeUtf8' bs of
          Left _ -> Left (TrasaErr N.status415 "Could note utf8 decode content type")
          Right typ -> Right (Content typ (N.responseBody response))
      where
        Config (Authority scheme host port) mmanager = config
        req = N.defaultRequest
          { N.method = TE.encodeUtf8 method
          , N.secure = schemeToSecure scheme
          , N.host = encodeAuthority host port
          , N.port = maybe (schemeToPort scheme) fromIntegral port
          , N.path = encodePathBS path
          , N.queryString = encodeQueryBS query
          , N.requestHeaders = [(N.hAccept,encodeAcceptBS accepts)] ++ case mcontent of
              Nothing -> []
              Just (Content typ _) -> [(N.hContentType,TE.encodeUtf8 typ)]
          , N.requestBody = case mcontent of
              Nothing -> N.RequestBodyLBS ""
              Just (Content _ reqBody) -> N.RequestBodyLBS reqBody
          }
