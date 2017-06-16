{-# LANGUAGE OverloadedStrings #-}
module Trasa.Codec
  (
  -- * Capture Codecs
    CaptureEncoding(..)
  , CaptureDecoding(..)
  , CaptureCodec(..)
  , captureCodecToCaptureEncoding
  , captureCodecToCaptureDecoding
  -- * Body Codecs
  , BodyEncoding(..)
  , BodyDecoding(..)
  , BodyCodec(..)
  , bodyCodecToBodyEncoding
  , bodyCodecToBodyDecoding
  -- * Type Class Based Codecs
  , showReadCaptureCodec
  , showReadBodyCodec
  ) where

import Text.Read (readMaybe,readEither)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import qualified Network.HTTP.Media.MediaType as N

newtype CaptureEncoding a = CaptureEncoding { appCaptureEncoding :: a -> T.Text }

newtype CaptureDecoding a = CaptureDecoding { appCaptureDecoding :: T.Text -> Maybe a }

data CaptureCodec a = CaptureCodec
  { captureCodecEncode :: a -> T.Text
  , captureCodecDecode :: T.Text -> Maybe a
  }

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

data BodyDecoding a = BodyDecoding
  { bodyDecodingNames :: NonEmpty N.MediaType
  , bodyDecodingFunction :: LBS.ByteString -> Either T.Text a
  }

data BodyCodec a = BodyCodec
  { bodyCodecNames :: NonEmpty N.MediaType
  , bodyCodecEncode :: a -> LBS.ByteString
  , bodyCodecDecode :: LBS.ByteString -> Either T.Text a
  }

bodyCodecToBodyEncoding :: BodyCodec a -> BodyEncoding a
bodyCodecToBodyEncoding (BodyCodec names enc _) = BodyEncoding names enc

bodyCodecToBodyDecoding :: BodyCodec a -> BodyDecoding a
bodyCodecToBodyDecoding (BodyCodec names _ dec) = BodyDecoding names dec

showReadBodyCodec :: (Show a, Read a) => BodyCodec a
showReadBodyCodec = BodyCodec
  (pure "text/haskell")
  (LBC.pack . show)
  (first T.pack . readEither . LBC.unpack)

