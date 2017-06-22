{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Trasa.Client
  (
  -- * Types
    Scheme(..)
  , Authority(..)
  , Config(..)
  -- * Requests
  , client
  , clientWith
  ) where

import Data.Word (Word16)
import Data.Semigroup ((<>))
import qualified Data.List.NonEmpty as NE
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
import qualified Network.HTTP.Media as N
import qualified Network.HTTP.Client as N

import Trasa.Core hiding (status,body)

-- | If you select Https you need to pass in a tls manager in config or tls wont actually happen
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
  , authorityPort :: !(Maybe Word16)
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

encodeAcceptBS :: NE.NonEmpty N.MediaType -> BS.ByteString
encodeAcceptBS = BS.intercalate "; " . fmap N.renderHeader . NE.toList

data Config = Config
  { configAuthority :: !Authority
  , configManager :: !N.Manager
  }

client
  :: ( HasMeta route
     , HasCaptureEncoding (CaptureStrategy route)
     , HasCaptureEncoding (QueryStrategy route)
     , RequestBodyStrategy route ~ Many requestBodyStrat
     , HasBodyEncoding requestBodyStrat
     , ResponseBodyStrategy route ~ Many responseBodyStrat
     , HasBodyDecoding responseBodyStrat
     )
  => Config
  -> Prepared route response
  -> IO (Either TrasaErr response)
client = clientWith (transformMeta . meta)
  where transformMeta = mapMeta captureEncoding captureEncoding (mapMany bodyEncoding) (mapMany bodyDecoding)

clientWith
  :: forall route response
  .  (forall caps qrys req resp. route caps qrys req resp -> MetaClient caps qrys req resp)
  -> Config
  -> Prepared route response
  -- ^ Which endpoint to request
  -> IO (Either TrasaErr response)
clientWith toMeta config =
  requestWith toMeta run
  where
    run :: Method -> Url -> Maybe Content -> NE.NonEmpty N.MediaType -> IO (Either TrasaErr Content)
    run method (Url path query) mcontent accepts  = do
      response <- N.httpLbs req manager
      let status = N.responseStatus response
          body   = N.responseBody response
      return $ case status < N.status400 of
        True -> case lookup N.hContentType (N.responseHeaders response) of
          Nothing -> Left (TrasaErr N.status415 "No content type found")
          Just bs -> case N.parseAccept bs of
            Nothing  -> Left (TrasaErr N.status415 "Could not decode content type")
            Just typ -> Right (Content typ body)
        False -> Left (TrasaErr status body)
      where
        Config (Authority scheme host port) manager = config
        req = N.defaultRequest
          { N.method = TE.encodeUtf8 $ encodeMethod method
          , N.secure = schemeToSecure scheme
          , N.host = encodeAuthority host port
          , N.port = maybe (schemeToPort scheme) fromIntegral port
          , N.path = encodePathBS path
          , N.queryString = encodeQueryBS query
          , N.requestHeaders = [(N.hAccept,encodeAcceptBS accepts)] ++ case mcontent of
              Nothing -> []
              Just (Content typ _) -> [(N.hContentType,N.renderHeader typ)]
          , N.requestBody = case mcontent of
              Nothing -> N.RequestBodyLBS ""
              Just (Content _ reqBody) -> N.RequestBodyLBS reqBody
          }
