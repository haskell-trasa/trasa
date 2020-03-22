{-# LANGUAGE OverloadedStrings #-}

module Trasa.Codec
  (
  -- * Capture Codecs
    CaptureEncoding(..)
  , HasCaptureEncoding(..)
  , CaptureDecoding(..)
  , HasCaptureDecoding(..)
  , CaptureCodec(..)
  , HasCaptureCodec(..)
  , captureCodecToCaptureEncoding
  , captureCodecToCaptureDecoding
  -- * Body Codecs
  , BodyEncoding(..)
  , HasBodyEncoding(..)
  , BodyDecoding(..)
  , HasBodyDecoding(..)
  , BodyCodec(..)
  , HasBodyCodec(..)
  , bodyCodecToBodyEncoding
  , bodyCodecToBodyDecoding
  -- * Type Class Based Codecs
  , showReadCaptureCodec
  , showReadBodyCodec
  , jsonCaptureCodec
  , jsonBodyCodec
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import Text.Read (readMaybe,readEither)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Media.MediaType as N

newtype CaptureEncoding a = CaptureEncoding { appCaptureEncoding :: a -> T.Text }

class HasCaptureEncoding capStrategy where
  captureEncoding :: capStrategy a -> CaptureEncoding a

instance HasCaptureEncoding CaptureEncoding where
  captureEncoding = id

newtype CaptureDecoding a = CaptureDecoding { appCaptureDecoding :: T.Text -> Maybe a }

class HasCaptureDecoding capStrategy where
  captureDecoding :: capStrategy a -> CaptureDecoding a

instance HasCaptureDecoding CaptureDecoding where
  captureDecoding = id

data CaptureCodec a = CaptureCodec
  { captureCodecEncode :: a -> T.Text
  , captureCodecDecode :: T.Text -> Maybe a
  }

class HasCaptureCodec capStrategy where
  captureCodec :: capStrategy a -> CaptureCodec a

instance HasCaptureEncoding CaptureCodec where
  captureEncoding = captureCodecToCaptureEncoding

instance HasCaptureDecoding CaptureCodec where
  captureDecoding = captureCodecToCaptureDecoding

instance HasCaptureCodec CaptureCodec where
  captureCodec = id

captureCodecToCaptureEncoding :: CaptureCodec a -> CaptureEncoding a
captureCodecToCaptureEncoding (CaptureCodec enc _) = CaptureEncoding enc

captureCodecToCaptureDecoding :: CaptureCodec a -> CaptureDecoding a
captureCodecToCaptureDecoding (CaptureCodec _ dec) = CaptureDecoding dec

showReadCaptureCodec :: (Show a, Read a) => CaptureCodec a
showReadCaptureCodec = CaptureCodec (T.pack . show) (readMaybe . T.unpack)

data BodyEncoding a = BodyEncoding
  { bodyEncodingNames :: NonEmpty N.MediaType
  , bodyEncodingFunction :: a -> LBS.ByteString
  }

class HasBodyEncoding bodyStrategy where
  bodyEncoding :: bodyStrategy a -> BodyEncoding a

instance HasBodyEncoding BodyEncoding where
  bodyEncoding = id

data BodyDecoding a = BodyDecoding
  { bodyDecodingNames :: NonEmpty N.MediaType
  , bodyDecodingFunction :: LBS.ByteString -> Either T.Text a
  }

class HasBodyDecoding bodyStrategy where
  bodyDecoding :: bodyStrategy a -> BodyDecoding a

instance HasBodyDecoding BodyDecoding where
  bodyDecoding = id

data BodyCodec a = BodyCodec
  { bodyCodecNames :: NonEmpty N.MediaType
  , bodyCodecEncode :: a -> LBS.ByteString
  , bodyCodecDecode :: LBS.ByteString -> Either T.Text a
  }

class HasBodyCodec bodyStrategy where
  bodyCodec :: bodyStrategy a -> BodyCodec a

instance HasBodyEncoding BodyCodec where
  bodyEncoding = bodyCodecToBodyEncoding

instance HasBodyDecoding BodyCodec where
  bodyDecoding = bodyCodecToBodyDecoding

instance HasBodyCodec BodyCodec where
  bodyCodec = id

bodyCodecToBodyEncoding :: BodyCodec a -> BodyEncoding a
bodyCodecToBodyEncoding (BodyCodec names enc _) = BodyEncoding names enc

bodyCodecToBodyDecoding :: BodyCodec a -> BodyDecoding a
bodyCodecToBodyDecoding (BodyCodec names _ dec) = BodyDecoding names dec

showReadBodyCodec :: (Show a, Read a) => BodyCodec a
showReadBodyCodec = BodyCodec
  (pure "text/haskell")
  (LBC.pack . show)
  (first T.pack . readEither . LBC.unpack)

jsonCaptureCodec :: (FromJSON a, ToJSON a) => CaptureCodec a
jsonCaptureCodec = CaptureCodec (T.decodeUtf8 . LBC.toStrict . A.encode) (A.decodeStrict . T.encodeUtf8)

jsonBodyCodec :: (FromJSON a, ToJSON a) => BodyCodec a
jsonBodyCodec = BodyCodec
  (pure "application/json; charset=utf-8")
  A.encode
  (first T.pack . A.eitherDecode)
