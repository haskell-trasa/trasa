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
import qualified Data.Text as T

import Data.Vinyl (Rec)
import Trasa.Core
import qualified Trasa.Method as M
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

data Route :: [Type] -> [Param] -> Bodiedness -> Type -> Type where
  AddR :: Route '[] '[] (Body Person) Key
  EditR :: Route '[Key] '[] (Body Person) ()
  DeleteR :: Route '[Key] '[] Bodyless ()
  ViewR :: Route '[Key] '[] Bodyless Person
  ViewAllR :: Route '[] '[Optional Int] Bodyless [Keyed Person]

data Meta ps qs rq rp = Meta
  { metaPath :: Path CaptureCodec ps
  , metaQuery :: Rec (Query CaptureCodec) qs
  , metaRequestBody :: RequestBody BodyCodec rq
  , metaResponseBody :: ResponseBody BodyCodec rp
  , metaMethod :: Method
  }

meta :: Route ps qs rq rp -> Meta ps qs rq rp
meta = \case
  AddR -> Meta (match "add" ./ end) qend (body bodyPerson) (resp bodyKey) M.post
  EditR -> Meta (match "edit" ./ capture key ./ end) qend (body bodyPerson) (resp bodyUnit) M.put
  DeleteR -> Meta (match "delete" ./ capture key ./ end) qend bodyless (resp bodyUnit) M.delete
  ViewR -> Meta (match "view" ./ capture key ./ end) qend bodyless (resp bodyPerson) M.get
  ViewAllR -> Meta (match "view-all" ./ end) (optional "limit" int .& qend) bodyless (resp bodyKeyed) M.get

key :: CaptureCodec Key
key = showReadCaptureCodec

int :: CaptureCodec Int
int = showReadCaptureCodec

bodyKey :: BodyCodec Key
bodyKey = showReadBodyCodec

bodyKeyed :: (Show a, Read a) => BodyCodec [Keyed a]
bodyKeyed = showReadBodyCodec

bodyPerson :: BodyCodec Person
bodyPerson = showReadBodyCodec

bodyUnit :: BodyCodec ()
bodyUnit = BodyCodec (pure "text/plain") (const "") (const (Right ()))

allRoutes :: [Constructed Route]
allRoutes = [Constructed AddR, Constructed EditR, Constructed DeleteR, Constructed ViewR, Constructed ViewAllR]

prepare :: Route cs qs rq rp -> Arguments cs qs rq (Prepared Route rp)
prepare = prepareWith (metaPath . meta) (metaQuery . meta) (metaRequestBody . meta)

link :: Prepared Route rp -> Url
link =
  linkWith (mapPath captureCodecToCaptureEncoding . metaPath . meta)
           (mapQuery captureCodecToCaptureEncoding . metaQuery . meta)
