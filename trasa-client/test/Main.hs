{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-unticked-promoted-constructors #-}
module Main where

import Data.Kind (Type)
import GHC.Generics hiding (Meta)
import Data.Bifunctor (Bifunctor(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Aeson
  (Value(..),FromJSON(..),ToJSON(..),encode,eitherDecode'
  ,object,withObject,(.:),(.=))
import Net.Types (IPv4)
import qualified Net.IPv4.String as IPv4
import Control.Exception (catch,SomeException)
import System.Exit (exitFailure)
import qualified Network.HTTP.Types.Status as N
import qualified Network.HTTP.Client as N
import Trasa.Core
import qualified Trasa.Method as M
import Trasa.Client

data Ip = Ip
  { origin :: IPv4
  } deriving (Generic,FromJSON,ToJSON)

instance Show Ip where
  show (Ip ipv4) = "{ origin: " ++ IPv4.encode ipv4 ++ " }"

data Args = Args
  { args :: H.HashMap T.Text Value
  } deriving Show

instance FromJSON Args where
  parseJSON = withObject "Args" $ \o -> Args <$> o .: "args"

instance ToJSON Args where
  toJSON (Args as) = object [ "args" .= as ]

bodyAeson :: (FromJSON a, ToJSON a) => BodyCodec a
bodyAeson = BodyCodec (pure "application/json") encode (first T.pack . eitherDecode')

int :: CaptureCodec Int
int = showReadCaptureCodec

bodyUnit :: BodyCodec ()
bodyUnit = BodyCodec (pure "text/html; charset=utf-8") (const "") (const (Right ()))

data Route :: [Type] -> [Param] -> Bodiedness -> Type -> Type where
  RouteHome :: Route '[] '[] Bodyless ()
  RouteIp :: Route '[] '[] Bodyless Ip
  RouteStatus :: Route '[Int] '[] Bodyless ()
  RouteQuery :: Route '[] '[Optional Int] Bodyless Args

data Meta caps qrys req resp = Meta
  { metaPath :: Path CaptureCodec caps
  , metaQuery :: Rec (Query CaptureCodec) qrys
  , metaRequestBody :: RequestBody BodyCodec req
  , metaResponseBody :: ResponseBody BodyCodec resp
  , metaMethod :: Method }

meta :: Route caps qrys req resp -> Meta caps qrys req resp
meta = \case
  RouteHome -> Meta end qend bodyless (resp bodyUnit) M.get
  RouteIp -> Meta (match "ip" ./ end) qend bodyless (resp bodyAeson) M.get
  RouteStatus -> Meta (match "status" ./ capture int ./ end) qend bodyless (resp bodyUnit) M.get
  RouteQuery -> Meta (match "anything" ./ end) (optional "int" int .& qend) bodyless (resp bodyAeson) M.get

prepare :: Route caps qrys req resp -> Arguments caps qrys req (Prepared Route resp)
prepare = prepareWith (metaPath . meta) (metaQuery . meta) (metaRequestBody . meta)

link :: Prepared Route resp -> Url
link = linkWith
  (mapPath captureCodecToCaptureEncoding . metaPath . meta)
  (mapQuery captureCodecToCaptureEncoding . metaQuery . meta)

client :: Config -> Prepared Route resp -> IO (Either TrasaErr resp)
client = clientWith
  (metaMethod . meta)
  (mapPath captureCodecToCaptureEncoding . metaPath . meta)
  (mapQuery captureCodecToCaptureEncoding . metaQuery . meta)
  (mapRequestBody (Many . pure . bodyCodecToBodyEncoding) . metaRequestBody . meta)
  (mapResponseBody (Many . pure . bodyCodecToBodyDecoding) . metaResponseBody . meta)

shouldRight :: Show resp => Config -> Prepared Route resp -> IO ()
shouldRight conf route = do
  putStr $ show (link route) ++ ": "
  client conf route >>= \case
    Left err -> do
      print err
      exitFailure
    Right val -> print val

main :: IO ()
main = do
  manager <- N.newManager N.defaultManagerSettings
  let conf = Config (Authority Http "httpbin.org" Nothing) manager
  res <- catch (client conf (prepare RouteHome)) $ \(_ :: SomeException) -> return (Left (status N.status400))
  case res of
    Left _  -> putStrLn "Could not connect to httpbin.org, not running test suite"
    Right _ -> do
      putStrLn "Connected to httpbin.org, actually testing routes now..."
      shouldRight conf (prepare RouteIp)
      shouldRight conf (prepare RouteStatus 200)
      shouldRight conf (prepare RouteQuery (Just 1))
