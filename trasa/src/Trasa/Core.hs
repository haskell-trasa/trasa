{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Trasa.Core
  (
  -- * Types
    Bodiedness(..)
  , Path(..)
  , ResponseBody(..)
  , RequestBody(..)
  , BodyCodec(..)
  , BodyDecoding(..)
  , BodyEncoding(..)
  , Many(..)
  , CaptureCodec(..)
  , CaptureEncoding(..)
  , CaptureDecoding(..)
  , Content(..)
  , Payload(..)
  , TrasaErr(..)
  -- ** Existential
  , Prepared(..)
  , Concealed(..)
  , Constructed(..)
  -- * Using Routes
  , prepareWith
  , dispatchWith
  , parseWith
  , linkWith
  , payloadWith
  , requestWith
  , handler
  -- * Defining Routes
  -- ** Path
  , match
  , capture
  , end
  , (./)
  -- ** Request Body
  , body
  , bodyless
  -- ** Response Body
  , resp
  -- * Converting Route Metadata
  , mapPath
  , mapRequestBody
  , mapResponseBody
  , bodyCodecToBodyEncoding
  , bodyCodecToBodyDecoding
  -- * Argument Currying
  , Arguments
  -- * Random Stuff
  , conceal
  , encodeRequestBody
  , decodeResponseBody
  -- * Show/Read Codecs
  , showReadBodyCodec
  , showReadCaptureCodec
  ) where

import Data.Kind (Type)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (mapMaybe,listToMaybe)
import Control.Monad
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Foldable (toList)
import Data.Bifunctor (first)
import Text.Read (readEither,readMaybe)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vinyl (Rec(..))

data Bodiedness = Body Type | Bodyless

data RequestBody :: (Type -> Type) -> Bodiedness -> Type where
  RequestBodyPresent :: f a -> RequestBody f ('Body a)
  RequestBodyAbsent :: RequestBody f 'Bodyless

mapRequestBody :: (forall x. rqf x -> rqf' x) -> RequestBody rqf rq -> RequestBody rqf' rq
mapRequestBody _ RequestBodyAbsent = RequestBodyAbsent
mapRequestBody f (RequestBodyPresent reqBod) = RequestBodyPresent (f reqBod)

newtype ResponseBody rpf rp = ResponseBody { getResponseBody :: rpf rp }

mapResponseBody :: (forall x. rpf x -> rpf' x) -> ResponseBody rpf rq -> ResponseBody rpf' rq
mapResponseBody f (ResponseBody resBod) = ResponseBody (f resBod)

data Path :: (Type -> Type) -> [Type] -> Type where
  PathNil :: Path cap '[]
  PathConsCapture :: cap a -> Path cap as -> Path cap (a ': as)
  PathConsMatch :: T.Text -> Path cap as -> Path cap as

mapPath :: (forall x. cf x -> cf' x) -> Path cf ps -> Path cf' ps
mapPath _ PathNil = PathNil
mapPath f (PathConsMatch s pnext) = PathConsMatch s (mapPath f pnext)
mapPath f (PathConsCapture c pnext) = PathConsCapture (f c) (mapPath f pnext)

newtype Many f a = Many { getMany :: NonEmpty (f a) }
  deriving (Functor)

data BodyDecoding a = BodyDecoding
  { bodyDecodingNames :: NonEmpty T.Text
  , bodyDecodingFunction :: LBS.ByteString -> Either T.Text a
  }

data BodyEncoding a = BodyEncoding
  { bodyEncodingNames :: NonEmpty T.Text
  , bodyEncodingFunction :: a -> LBS.ByteString
  }

-- Note to self, we maybe should change this to use list instead of
-- non-empty list. When encoding unit, we actually want
-- to omit the Content-Type header.
data BodyCodec a = BodyCodec
  { bodyCodecNames :: NonEmpty T.Text
  , bodyCodecEncode :: a -> LBS.ByteString
  , bodyCodecDecode :: LBS.ByteString -> Either T.Text a
  }

bodyCodecToBodyEncoding :: BodyCodec a -> BodyEncoding a
bodyCodecToBodyEncoding (BodyCodec names enc _) = BodyEncoding names enc

bodyCodecToBodyDecoding :: BodyCodec a -> BodyDecoding a
bodyCodecToBodyDecoding (BodyCodec names _ dec) = BodyDecoding names dec

infixr 7 ./

(./) :: (a -> b) -> a -> b
(./) f a = f a

match :: T.Text -> Path cpf ps -> Path cpf ps
match = PathConsMatch

capture :: cpf cp -> Path cpf cps -> Path cpf (cp ': cps)
capture = PathConsCapture

end :: Path cpf '[]
end = PathNil

body :: rqf rq -> RequestBody rqf ('Body rq)
body = RequestBodyPresent

bodyless :: RequestBody rqf 'Bodyless
bodyless = RequestBodyAbsent

resp :: rpf rp -> ResponseBody rpf rp
resp = ResponseBody

-- data Fragment (x :: [Type] -> [Type]) where
--   FragmentCapture :: Capture a -> Fragment ('(:) a)

data CaptureCodec a = CaptureCodec
  { captureCodecEncode :: a -> T.Text
  , captureCodecDecode :: T.Text -> Maybe a
  }

newtype CaptureEncoding a = CaptureEncoding { appCaptureEncoding :: a -> T.Text }
newtype CaptureDecoding a = CaptureDecoding { appCaptureDecoding :: T.Text -> Maybe a }

-- | This does not use the request body since the request body
--   does not appear in a URL.
linkWith :: forall rt rp.
     (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureEncoding cs')
  -> Prepared rt rp
  -> [T.Text]
linkWith toCapEncs (Prepared route captures _) = encodePieces (toCapEncs route) captures
-- We should probably go ahead and just URL encode the path 
-- when someone calls linkWith.

data Payload = Payload
  { payloadPath :: [T.Text]
  , payloadContent :: Maybe Content
  , payloadAccepts :: NonEmpty T.Text
  } deriving (Show,Eq,Ord)

payloadWith :: forall rt rp.
     (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureEncoding cs')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyEncoding) rq')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody (Many BodyDecoding) rp')
  -> Prepared rt rp
  -> Payload
payloadWith toCapEncs toReqBody toRespBody p@(Prepared route _ reqBody) =
  Payload (linkWith toCapEncs p) content accepts
  where content = encodeRequestBody (toReqBody route) reqBody
        ResponseBody (Many decodings) = toRespBody route
        accepts = bodyDecodingNames =<< decodings

requestWith :: Functor m
  => (forall cs' rq' rp'. rt cs' rq' rp' -> T.Text)
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureEncoding cs')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyEncoding) rq')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody (Many BodyDecoding) rp')
  -> (T.Text -> [T.Text] -> Maybe Content -> [T.Text] -> m Content) -- ^ method, path pieces, content, accepts -> response
  -> Prepared rt rp
  -> m (Maybe rp)
requestWith toMethod toCapEncs toReqBody toRespBody run (Prepared route captures reqBody) =
  let method = toMethod route
      encodedCaptures = encodePieces (toCapEncs route) captures
      content = encodeRequestBody (toReqBody route) reqBody
      respBodyDecs = toRespBody route
      ResponseBody (Many decodings) = respBodyDecs
      accepts = toList (bodyDecodingNames =<< decodings)
   in fmap (decodeResponseBody respBodyDecs) (run method encodedCaptures content accepts)

encodeRequestBody :: RequestBody (Many BodyEncoding) rq -> RequestBody Identity rq -> Maybe Content
encodeRequestBody RequestBodyAbsent RequestBodyAbsent = Nothing
encodeRequestBody (RequestBodyPresent (Many encodings)) (RequestBodyPresent (Identity rq)) =
  case NE.head encodings of
    BodyEncoding names encoding -> Just (Content (NE.head names) (encoding rq))

-- TODO: Add better error handling ie: Either TrasaErr rp
decodeResponseBody :: ResponseBody (Many BodyDecoding) rp -> Content -> Maybe rp
decodeResponseBody (ResponseBody (Many decodings)) (Content name content) =
  flip mapFind decodings $ \(BodyDecoding names decode) ->
    if elem name names then hush (decode content) else Nothing
  where hush (Left _)  = Nothing
        hush (Right a) = Just a

encodePieces :: Path CaptureEncoding cps -> Rec Identity cps -> [T.Text]
encodePieces = go
  where
  go :: forall cps. Path CaptureEncoding cps -> Rec Identity cps -> [T.Text]
  go PathNil RNil = []
  go (PathConsMatch str ps) xs = str : go ps xs
  go (PathConsCapture (CaptureEncoding enc) ps) (Identity x :& xs) = enc x : go ps xs

data TrasaErr = TrasaErr
  { trasaErrHTTPCode :: Int
  , trasaErrPhrase :: T.Text
  , trasaErrBody :: LBS.ByteString
  } deriving (Show,Eq,Ord)

err404 :: TrasaErr
err404 = TrasaErr 404 "Not Found" ""

err400 :: TrasaErr
err400 = TrasaErr 400 "Bad Request" ""

err406 :: TrasaErr
err406 = TrasaErr 406 "Not Acceptable" ""

err415 :: TrasaErr
err415 = TrasaErr 415 "Unsupported Media Type" ""

dispatchWith :: forall rt m.
     Applicative m
  => (forall cs' rq' rp'. rt cs' rq' rp' -> T.Text) -- ^ Method
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureDecoding cs')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyDecoding) rq')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody (Many BodyEncoding) rp')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Rec Identity cs' -> RequestBody Identity rq' -> m rp')
  -> [Constructed rt] -- ^ All available routes
  -> T.Text -- ^ Method
  -> [T.Text] -- ^ Accept headers
  -> [T.Text] -- ^ Path Pieces
  -> Maybe Content -- ^ Content type and request body
  -> m (Either TrasaErr LBS.ByteString) -- ^ Encoded response
dispatchWith toMethod toCapDec toReqBody toRespBody makeResponse enumeratedRoutes method accepts encodedPath mcontent = sequenceA $ do
  Concealed route decodedPathPieces decodedRequestBody <- parseWith
    toMethod toCapDec toReqBody enumeratedRoutes method encodedPath mcontent
  let response = makeResponse route decodedPathPieces decodedRequestBody
      ResponseBody (Many encodings) = toRespBody route
  encode <- mapFindE err406
    (\(BodyEncoding names encode) ->
       if any (flip elem accepts) names then Just encode else Nothing)
    encodings
  Right (fmap encode response)

-- | This function is exported largely for illustrative purposes.
--   In actual applications, 'dispatchWith' would be used more often.
parseWith :: forall rt.
     (forall cs' rq' rp'. rt cs' rq' rp' -> T.Text) -- ^ Method
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureDecoding cs')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyDecoding) rq')
  -> [Constructed rt] -- ^ All available routes
  -> T.Text -- ^ Request Method
  -> [T.Text] -- ^ Path Pieces
  -> Maybe Content -- ^ Request content type and body
  -> Either TrasaErr (Concealed rt)
parseWith toMethod toCapDec toReqBody enumeratedRoutes method encodedPath mcontent = do
  Pathed route captures <- mapFindE err404
    (\(Constructed route) -> do
      guard (toMethod route == method)
      fmap (Pathed route) (parseOne (toCapDec route) encodedPath)
    ) enumeratedRoutes
  decodedRequestBody <- case toReqBody route of
    RequestBodyPresent (Many decodings) -> case mcontent of
      Just (Content typ encodedRequest) -> do
        decode <- mapFindE err415 (\(BodyDecoding names decode) -> if elem typ names then Just decode else Nothing) decodings
        reqVal <- badReq (decode encodedRequest)
        Right (RequestBodyPresent (Identity reqVal))
      Nothing -> Left err415
    RequestBodyAbsent -> case mcontent of
      Just _ -> Left err415
      Nothing -> Right RequestBodyAbsent
  return (Concealed route captures decodedRequestBody)
  where badReq :: Either T.Text b -> Either TrasaErr b
        badReq = first (\t -> err400 { trasaErrBody = LBS.fromStrict (T.encodeUtf8 t) })

parseOne ::
     Path CaptureDecoding cps
  -> [T.Text] -- ^ Path Pieces
  -> Maybe (Rec Identity cps)
parseOne = go
  where
  go :: forall cps. Path CaptureDecoding cps -> [T.Text] -> Maybe (Rec Identity cps)
  go PathNil xs = case xs of
    [] -> Just RNil
    _ : _ -> Nothing
  go (PathConsMatch s psNext) xs = case xs of
    [] -> Nothing
    x : xsNext -> if x == s
      then go psNext xsNext
      else Nothing
  go (PathConsCapture (CaptureDecoding decode) psNext) xs = case xs of
    [] -> Nothing
    x : xsNext -> do
      v <- decode x
      vs <- go psNext xsNext
      Just (Identity v :& vs)

type family Arguments (pieces :: [Type]) (body :: Bodiedness) (result :: Type) :: Type where
  Arguments '[] ('Body b) r = b -> r
  Arguments '[] 'Bodyless r = r
  Arguments (c ': cs) b r = c -> Arguments cs b r

prepareWith ::
     (forall cs' rq' rp'. rt cs' rq' rp' -> Path pf cs')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody rqf rq')
  -> rt cs rq rp
  -> Arguments cs rq (Prepared rt rp)
prepareWith toPath toReqBody route =
  prepareExplicit route (toPath route) (toReqBody route)

prepareExplicit :: forall rt cs rq rp rqf pf.
     rt cs rq rp
  -> Path pf cs
  -> RequestBody rqf rq
  -> Arguments cs rq (Prepared rt rp)
prepareExplicit route = go (Prepared route)
  where
  -- Adopted from: https://www.reddit.com/r/haskell/comments/67l9so/currying_a_typelevel_list/dgrghxz/
  go :: forall cs' z.
        (Rec Identity cs' -> RequestBody Identity rq -> z)
     -> Path pf cs'
     -> RequestBody rqf rq
     -> Arguments cs' rq z -- (HList cs', RequestBody Identity rq)
  go k (PathConsCapture _ pnext) b = \c -> go (\hlist reqBod -> k (Identity c :& hlist) reqBod) pnext b
  go k (PathConsMatch _ pnext) b = go k pnext b
  go k PathNil RequestBodyAbsent = k RNil RequestBodyAbsent
  go k PathNil (RequestBodyPresent _) = \reqBod -> k RNil (RequestBodyPresent (Identity reqBod))

handler :: forall cs rq x. 
     Rec Identity cs 
  -> RequestBody Identity rq 
  -> Arguments cs rq x 
  -> x
handler = go
  where
  go :: forall cs'. Rec Identity cs' -> RequestBody Identity rq -> Arguments cs' rq x -> x
  go (Identity c :& cs) b f = go cs b (f c)
  go RNil RequestBodyAbsent f = f
  go RNil (RequestBodyPresent (Identity b)) f = f b


data Constructed :: ([Type] -> Bodiedness -> Type -> Type) -> Type where
  Constructed :: forall rt cps rq rp. rt cps rq rp -> Constructed rt

-- | Only includes the path. Once querystring params get added
--   to this library, this data type should not have them.
data Pathed :: ([Type] -> Bodiedness -> Type -> Type) -> Type  where
  Pathed :: forall rt cps rq rp. rt cps rq rp -> Rec Identity cps -> Pathed rt

-- | Includes the path and the request body (and the querystring
--   params after they get added to this library).
data Prepared :: ([Type] -> Bodiedness -> Type -> Type) -> Type -> Type where
  Prepared :: forall rt ps rq rp.
       rt ps rq rp
    -> Rec Identity ps
    -> RequestBody Identity rq
    -> Prepared rt rp

-- | Only needed to implement 'parseWith'. Most users do not need this.
--   If you need to create a route hierarchy to provide breadcrumbs,
--   then you will need this.
data Concealed :: ([Type] -> Bodiedness -> Type -> Type) -> Type where
  Concealed :: forall rt ps rq rp.
       rt ps rq rp
    -> Rec Identity ps
    -> RequestBody Identity rq
    -> Concealed rt

conceal :: Prepared rt rp -> Concealed rt
conceal (Prepared a b c) = Concealed a b c

data Content = Content
  { contentType :: T.Text
  , contentData :: LBS.ByteString
  } deriving (Show,Eq,Ord)

mapFind :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
mapFind f = listToMaybe . mapMaybe f . toList

mapFindE :: Foldable f => e -> (a -> Maybe b) -> f a -> Either e b
mapFindE e f = listToEither . mapMaybe f . toList
  where listToEither [] = Left e
        listToEither (x:_) = Right x

showReadBodyCodec :: (Show a, Read a) => BodyCodec a
showReadBodyCodec = BodyCodec 
  (pure "text/haskell")
  (LBC.pack . show)
  (first T.pack . readEither . LBC.unpack)

showReadCaptureCodec :: (Show a, Read a) => CaptureCodec a
showReadCaptureCodec = CaptureCodec (T.pack . show) (readMaybe . T.unpack)

