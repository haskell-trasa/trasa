{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-unticked-promoted-constructors #-}
module Common where

import Data.Kind (Type)

import Data.Bifunctor (first)
import Text.Read (readMaybe,readEither)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T

import Trasa.Core

newtype Key = Key Int deriving (Enum, Eq, Ord)

instance Show Key where
  show (Key i) = show i

instance Read Key where
  readsPrec p = fmap (first Key) . readsPrec p

data Keyed a = Keyed
  { keyedKey :: Key
  , keyedValue :: a
  } deriving (Show, Read)


data Person = Person
  { personAge :: Int
  , personName :: T.Text
  } deriving (Show, Read)

data Route :: [Type] -> Bodiedness Type -> Type -> Type where
  AddR :: Route '[] (Body Person) Key
  EditR :: Route '[Key] (Body Person) ()
  DeleteR :: Route '[Key] Bodyless ()
  ViewR :: Route '[Key] Bodyless Person
  ViewAllR :: Route '[] Bodyless [Keyed Person]

data Meta ps rq rp = Meta
  { metaPath :: Path CaptureCodec ps
  , metaRequestBody :: RequestBody BodyCodec rq
  , metaResponseBody :: ResponseBody BodyCodec rp
  , metaMethod :: T.Text
  }

meta :: Route ps rq rp -> Meta ps rq rp
meta = \case
  AddR -> Meta (match "add" ./ end) (body bodyPerson) (resp bodyKey) "POST"
  EditR -> Meta (match "edit" ./ capture key ./ end) (body bodyPerson) (resp bodyUnit) "PUT"
  DeleteR -> Meta (match "delete" ./ capture key ./ end) bodyless (resp bodyUnit) "DELETE"
  ViewR -> Meta (match "view" ./ capture key ./ end) bodyless (resp bodyPerson) "GET"
  ViewAllR -> Meta (match "view-all" ./ end) bodyless (resp bodyKeyed) "GET"

encodeCapture :: Show a => a -> T.Text
encodeCapture = T.pack . show

decodeCapture :: Read a => T.Text -> Maybe a
decodeCapture = readMaybe . T.unpack

key :: CaptureCodec Key
key = CaptureCodec encodeCapture decodeCapture

textPlain :: Applicative f => f T.Text
textPlain = pure "text/plain"

encodeBody :: Show a => a -> LBS.ByteString
encodeBody = LBS.pack . show

decodeBody :: Read a => LBS.ByteString -> Either T.Text a
decodeBody = first T.pack . readEither . LBS.unpack

bodyKey :: BodyCodec Key
bodyKey = BodyCodec textPlain encodeBody decodeBody

bodyKeyed :: (Show a, Read a) => BodyCodec [Keyed a]
bodyKeyed = BodyCodec textPlain encodeBody decodeBody

bodyPerson :: BodyCodec Person
bodyPerson = BodyCodec textPlain encodeBody decodeBody

bodyUnit :: BodyCodec ()
bodyUnit = BodyCodec textPlain encodeBody decodeBody

allRoutes :: [Constructed Route]
allRoutes = [Constructed AddR, Constructed EditR, Constructed DeleteR, Constructed ViewR, Constructed ViewAllR]

prepare :: Route cs rq rp -> Arguments cs rq (Prepared Route rp)
prepare = prepareWith (metaPath . meta) (metaRequestBody . meta)

link :: Prepared Route rp -> [T.Text]
link = linkWith (mapPath (CaptureEncoding . captureCodecEncode) . metaPath . meta)
