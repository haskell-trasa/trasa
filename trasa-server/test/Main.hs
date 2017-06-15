{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Trasa.Core
import Trasa.Server
import Data.Vinyl
import Data.Functor.Identity
import Data.Kind (Type)
import Text.Read (readMaybe)
import Network.Wai.Handler.Warp (withApplication)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Network.HTTP.Client as HC

main :: IO ()
main = do
  putStrLn "\nStarting trasa server test suite"
  let app = serve
        (mapQuerys captureCodecToCaptureDecoding . metaQuery . meta)
        (mapRequestBody (Many . pure . bodyCodecToBodyDecoding) . metaRequestBody . meta)
        (mapResponseBody (Many . pure . bodyCodecToBodyEncoding) . metaResponseBody . meta)
        handle
        router
  withApplication (return app) $ \port -> do
    manager <- HC.newManager HC.defaultManagerSettings
    attempt manager ("GET http://127.0.0.1:" ++ show port ++ "/") $ \x -> x
      { HC.requestHeaders = [("Accept","text/plain"),("ContentType","text/plain")]
      }
    attempt manager ("GET http://127.0.0.1:" ++ show port ++ "/hello") $ \x -> x
      { HC.requestHeaders = [("Accept","text/plain"),("ContentType","text/plain")]
      }
    return ()

attempt :: HC.Manager -> String -> (HC.Request -> HC.Request) -> IO ()
attempt mngr url modify = do
  req <- HC.parseUrlThrow url
  let req' = modify req
  _ <- HC.httpLbs req' mngr
  return ()

handle :: Route caps qrys req resp -> Rec Identity caps -> Rec Parameter qrys -> RequestBody Identity req -> TrasaT IO resp
handle r = case r of
  EmptyR -> \_ _ _ -> return (55 :: Int)
  HelloR -> \_ _ _ -> return (67 :: Int)

data Route :: [Type] -> [Param] -> Bodiedness -> Type -> Type where
  EmptyR :: Route '[] '[] Bodyless Int
  HelloR :: Route '[] '[] Bodyless Int
  AdditionR :: Route '[Int,Int] '[Optional Int] Bodyless Int
  IdentityR :: Route '[String] '[] Bodyless String
  LeftPadR :: Route '[Int] '[] (Body String) String
  TrickyOneR :: Route '[Int] '[] Bodyless String
  TrickyTwoR :: Route '[Int,Int] '[] Bodyless String


prepare :: Route cs qs rq rp -> Arguments cs qs rq (Prepared Route rp)
prepare = prepareWith (metaPath . meta) (metaQuery . meta) (metaRequestBody . meta)

link :: Prepared Route rp -> Url
link = linkWith
  (mapPath (CaptureEncoding . captureCodecEncode) . metaPath . meta)
  (mapQuerys captureCodecToCaptureEncoding . metaQuery . meta)

parse :: T.Text -> Maybe Content -> Either TrasaErr (Concealed Route)
parse url = parseWith
  (mapQuerys captureCodecToCaptureDecoding . metaQuery . meta)
  (mapRequestBody (Many . pure . bodyCodecToBodyDecoding) . metaRequestBody . meta)
  router
  "GET"
  (decodeUrl url)

allRoutes :: [Constructed Route]
allRoutes = 
  [ Constructed HelloR
  , Constructed AdditionR
  , Constructed IdentityR
  , Constructed LeftPadR
  , Constructed TrickyOneR
  , Constructed TrickyTwoR
  , Constructed EmptyR
  ]

router :: Router Route
router = routerWith
  (metaMethod . meta)
  (mapPath (CaptureDecoding . captureCodecDecode) . metaPath . meta)
  allRoutes

data Meta ps qs rq rp = Meta
  { metaPath :: Path CaptureCodec ps
  , metaQuery :: Rec (Query CaptureCodec) qs
  , metaRequestBody :: RequestBody BodyCodec rq
  , metaResponseBody :: ResponseBody BodyCodec rp
  , metaMethod :: T.Text
  }

meta :: Route ps qs rq rp -> Meta ps qs rq rp
meta x = case x of
  EmptyR -> Meta
    end
    qend
    bodyless (resp bodyInt) "GET"
  HelloR -> Meta
    (match "hello" ./ end)
    qend
    bodyless (resp bodyInt) "GET"
  AdditionR -> Meta
    (match "add" ./ capture int ./ capture int ./ end)
    (optional "more" int .& qend)
    bodyless (resp bodyInt) "GET"
  IdentityR -> Meta
    (match "identity" ./ capture string ./ end)
    qend
    bodyless (resp bodyString) "GET"
  LeftPadR -> Meta
    (match "pad" ./ match "left" ./ capture int ./ end)
    qend
    (body bodyString) (resp bodyString) "GET"
  TrickyOneR -> Meta
    (match "tricky" ./ capture int ./ match "one" ./ end)
    qend
    bodyless (resp bodyString) "GET"
  TrickyTwoR -> Meta
    (capture int ./ capture int ./ match "two" ./ end)
    qend
    bodyless (resp bodyString) "GET"


int :: CaptureCodec Int
int = CaptureCodec (T.pack . show) (readMaybe . T.unpack)

string :: CaptureCodec String
string = CaptureCodec T.pack (Just . T.unpack)

bodyString :: BodyCodec String
bodyString = BodyCodec (pure "text/plain") LBSC.pack (Right . LBSC.unpack)

bodyUnit :: BodyCodec ()
bodyUnit = BodyCodec (pure "text/plain") (const "") (const (Right ()))

note :: e -> Maybe a -> Either e a
note e Nothing = Left e
note _ (Just x) = Right x

bodyInt :: BodyCodec Int
bodyInt = BodyCodec (pure "text/plain") (LBSC.pack . show)
                    (note "Could not decode int" . readMaybe . LBSC.unpack)
