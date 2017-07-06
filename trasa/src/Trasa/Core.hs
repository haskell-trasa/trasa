{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeInType #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-unticked-promoted-constructors #-}
module Trasa.Core
  (
  -- * Types
    Bodiedness(..)
  , Clarity(..)
  , Content(..)
  , Payload(..)
  , Router
  -- ** Existential
  , Prepared(..)
  , Concealed(..)
  , Constructed(..)
  , conceal
  , concealedToPrepared
  , mapConstructed
  -- * Request Types
  -- ** Method
  , Method
  , encodeMethod
  , decodeMethod
  -- ** Queries
  , QueryString(..)
  , encodeQuery
  , decodeQuery
  -- ** Url
  , Url(..)
  , encodeUrl
  , decodeUrl
  -- ** Errors
  , TrasaErr(..)
  , status
  -- * Using Routes
  , prepareWith
  , linkWith
  , parseWith
  , payloadWith
  , requestWith
  , routerWith
  -- * Defining Routes
  -- ** Path
  , Path(..)
  , match
  , capture
  , end
  , (./)
  , mapPath
  , appendPath
  -- ** Query
  , Param(..)
  , Query(..)
  , Parameter(..)
  , Rec(..)
  , demoteParameter
  , flag
  , optional
  , list
  , qend
  , (.&)
  , mapQuery
  -- ** Request Body
  , RequestBody(..)
  , body
  , bodyless
  , encodeRequestBody
  , decodeRequestBody
  , mapRequestBody
  -- ** Response Body
  , ResponseBody(..)
  , resp
  , raw
  , encodeResponseBody
  , decodeResponseBody
  , mapResponseBody
  -- ** Many
  , Many(..)
  , one
  , mapMany
  -- ** Meta
  , Meta(..)
  , MetaBuilder
  , metaBuilderToMetaCodec
  , MetaCodec
  , MetaClient
  , metaCodecToMetaClient
  , MetaServer
  , metaCodecToMetaServer
  , mapMetaPath
  , mapMetaQuery
  , mapMetaRequestBody
  , mapMetaResponseBody
  , mapMeta
  -- * Codecs
  , CaptureEncoding(..)
  , HasCaptureEncoding(..)
  , CaptureDecoding(..)
  , HasCaptureDecoding(..)
  , CaptureCodec(..)
  , HasCaptureCodec(..)
  , BodyEncoding(..)
  , HasBodyEncoding(..)
  , BodyDecoding(..)
  , HasBodyDecoding(..)
  , BodyCodec(..)
  , HasBodyCodec(..)
    -- ** Converting Codecs
  , captureCodecToCaptureEncoding
  , captureCodecToCaptureDecoding
  , bodyCodecToBodyEncoding
  , bodyCodecToBodyDecoding
  -- ** Type Class based Codecs
  , showReadCaptureCodec
  , showReadBodyCodec
  -- * Argument Currying
  , ParamBase
  , Arguments
  , handler
  -- * Helpers
  , prettyRouter
  ) where

import Data.Kind (Type)
import Data.Functor.Identity (Identity(..))
import Control.Applicative (liftA2)
import Data.Maybe (mapMaybe,listToMaybe,isJust)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Foldable (toList)
import Data.Bifunctor (first,bimap)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as N
import qualified Network.HTTP.Media.MediaType as N
import qualified Network.HTTP.Media.Accept as N
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Unsafe.Coerce (unsafeCoerce)
import Data.Type.Equality ((:~:)(..))
import Data.Vinyl (Rec(..),rmap,rtraverse)
import Data.Vinyl.TypeLevel (type (++))

import Trasa.Method
import Trasa.Url
import Trasa.Error
import Trasa.Codec

-- $setup
-- >>> :set -XTypeInType

newtype Many f a = Many { getMany :: NonEmpty (f a) }
  deriving (Functor)

instance Applicative f => Applicative (Many f) where
  pure = Many . pure . pure
  Many mf <*> Many mx = Many $ liftA2 (<*>) mf mx

one :: f a -> Many f a
one = Many . pure

mapMany :: (forall x. f x -> g x) -> Many f a -> Many g a
mapMany eta (Many m) = Many (fmap eta m)

data Bodiedness = forall a. Body a | Bodyless

data RequestBody :: (Type -> Type) -> Bodiedness -> Type where
  RequestBodyPresent :: !(f a) -> RequestBody f ('Body a)
  RequestBodyAbsent :: RequestBody f 'Bodyless

body :: rqf req -> RequestBody rqf ('Body req)
body = RequestBodyPresent

bodyless :: RequestBody rqf 'Bodyless
bodyless = RequestBodyAbsent

mapRequestBody :: (forall x. rqf x -> rqf' x) -> RequestBody rqf request -> RequestBody rqf' request
mapRequestBody _ RequestBodyAbsent = RequestBodyAbsent
mapRequestBody f (RequestBodyPresent reqBod) = RequestBodyPresent (f reqBod)

data Clarity (r :: Type) = forall a. Clear a | Raw

data ResponseBody :: (Type -> Type) -> Clarity r -> Type where
  ResponseBodyClear :: !(f a) -> ResponseBody f (Clear a)
  ResponseBodyRaw :: !r -> ResponseBody f (Raw :: Clarity r)

resp :: rpf resp -> ResponseBody rpf (Clear resp)
resp = ResponseBodyClear

raw :: a -> ResponseBody rpf (Raw :: Clarity a)
raw = ResponseBodyRaw

mapResponseBody :: (forall x. rpf x -> rpf' x) -> ResponseBody rpf request -> ResponseBody rpf' request
mapResponseBody f (ResponseBodyClear resBod) = ResponseBodyClear (f resBod)
mapResponseBody _ (ResponseBodyRaw r) = ResponseBodyRaw r

data Path :: (Type -> Type) -> [Type] -> Type where
  PathNil :: Path cap '[]
  PathConsCapture :: !(cap a) -> !(Path cap as) -> Path cap (a ': as)
  PathConsMatch :: !T.Text -> !(Path cap as) -> Path cap as

infixr 7 ./

(./) :: (a -> b) -> a -> b
(./) f a = f a

match :: T.Text -> Path cpf caps -> Path cpf caps
match = PathConsMatch

capture :: cpf cap -> Path cpf caps -> Path cpf (cap ': caps)
capture = PathConsCapture

end :: Path cpf '[]
end = PathNil

mapPath :: (forall x. cf x -> cf' x) -> Path cf ps -> Path cf' ps
mapPath _ PathNil = PathNil
mapPath f (PathConsMatch s pnext) = PathConsMatch s (mapPath f pnext)
mapPath f (PathConsCapture c pnext) = PathConsCapture (f c) (mapPath f pnext)

appendPath :: Path f as -> Path f bs -> Path f (as ++ bs)
appendPath PathNil bs = bs
appendPath (PathConsMatch a as) bs = PathConsMatch a (appendPath as bs)
appendPath (PathConsCapture cas as) bs = PathConsCapture cas (appendPath as bs)

data Param
  = Flag
  | forall a. Optional a
  | forall a. List a

data Parameter :: Param -> Type where
  ParameterFlag :: !Bool -> Parameter Flag
  ParameterOptional :: !(Maybe a) -> Parameter (Optional a)
  ParameterList :: ![a] -> Parameter (List a)

data Query :: (Type -> Type) -> Param -> Type where
  QueryFlag :: !T.Text -> Query cap Flag
  QueryOptional :: !T.Text -> !(cap a) -> Query cap (Optional a)
  QueryList :: !T.Text -> !(cap a) -> Query cap (List a)

flag :: T.Text -> Query cpf Flag
flag = QueryFlag

optional :: T.Text -> cpf query -> Query cpf (Optional query)
optional = QueryOptional

list :: T.Text -> cpf query -> Query cpf (List query)
list = QueryList

qend :: Rec (Query qpf) '[]
qend = RNil

infixr 7 .&

(.&) :: Query qpf q -> Rec (Query qpf) qs -> Rec (Query qpf) (q ': qs)
(.&) = (:&)

mapQuery :: (forall x. f x -> g x) -> Rec (Query f) qs -> Rec (Query g) qs
mapQuery eta = rmap $ \case
  QueryFlag key -> QueryFlag key
  QueryOptional key query -> QueryOptional key (eta query)
  QueryList key query -> QueryList key (eta query)

data Meta capCodec qryCodec reqCodec respCodec caps qrys req resp = Meta
  { metaPath :: !(Path capCodec caps)
  , metaQuery :: !(Rec (Query qryCodec) qrys)
  , metaRequestBody :: !(RequestBody reqCodec req)
  , metaResponseBody :: !(ResponseBody respCodec resp)
  , metaMethod :: !Method
  }

mapMetaPath
  :: (forall x. cf x -> cg x)
  -> Meta cf qryCodec reqCodec respCodec caps qrys req resp
  -> Meta cg qryCodec reqCodec respCodec caps qrys req resp
mapMetaPath eta m = m { metaPath = mapPath eta (metaPath m) }

mapMetaQuery
  :: (forall x. qf x -> qg x)
  -> Meta capCodec qf reqCodec respCodec caps qrys req resp
  -> Meta capCodec qg reqCodec respCodec caps qrys req resp
mapMetaQuery eta m = m { metaQuery = mapQuery eta (metaQuery m) }

mapMetaRequestBody
  :: (forall x. rf x -> rg x)
  -> Meta capCodec qryCodec rf respCodec caps qrys req resp
  -> Meta capCodec qryCodec rg respCodec caps qrys req resp
mapMetaRequestBody eta m = m { metaRequestBody = mapRequestBody eta (metaRequestBody m) }

mapMetaResponseBody
  :: (forall x. rf x -> rg x)
  -> Meta capCodec qryCodec reqCodec rf caps qrys req resp
  -> Meta capCodec qryCodec reqCodec rg caps qrys req resp
mapMetaResponseBody eta m = m { metaResponseBody = mapResponseBody eta (metaResponseBody m)}

mapMeta
  :: (forall x. capCodec1 x -> capCodec2 x)
  -> (forall x. qryCodec1 x -> qryCodec2 x)
  -> (forall x. reqCodec1 x -> reqCodec2 x)
  -> (forall x. respCodec1 x -> respCodec2 x)
  -> Meta capCodec1 qryCodec1 reqCodec1 respCodec1 caps qrys req resp
  -> Meta capCodec2 qryCodec2 reqCodec2 respCodec2 caps qrys req resp
mapMeta mapCaps mapQrys mapReq mapResp (Meta caps qrys req res method) = Meta
  (mapPath mapCaps caps)
  (mapQuery mapQrys qrys)
  (mapRequestBody mapReq req)
  (mapResponseBody mapResp res)
  method

type MetaBuilder = Meta CaptureCodec CaptureCodec BodyCodec BodyCodec

metaBuilderToMetaCodec :: MetaBuilder caps qrys req resp -> MetaCodec caps qrys req resp
metaBuilderToMetaCodec (Meta path query reqBody respBody method) = Meta
  path
  query
  (mapRequestBody one reqBody)
  (mapResponseBody one respBody)
  method

type MetaCodec = Meta CaptureCodec CaptureCodec (Many BodyCodec) (Many BodyCodec)

type MetaClient = Meta CaptureEncoding CaptureEncoding (Many BodyEncoding) (Many BodyDecoding)

metaCodecToMetaClient :: MetaCodec caps qrys req resp -> MetaClient caps qrys req resp
metaCodecToMetaClient = mapMeta captureEncoding captureEncoding (mapMany bodyEncoding) (mapMany bodyDecoding)

type MetaServer = Meta CaptureDecoding CaptureDecoding (Many BodyDecoding) (Many BodyEncoding)

metaCodecToMetaServer :: MetaCodec caps qrys req resp -> MetaServer caps qrys req resp
metaCodecToMetaServer = mapMeta captureDecoding captureDecoding (mapMany bodyDecoding) (mapMany bodyEncoding)

-- | Generate a @Url@ for use in hyperlinks.
linkWith
  :: forall route response reqCodec respCodec
  .  (forall caps qrys req resp. route caps qrys req resp -> Meta CaptureEncoding CaptureEncoding reqCodec respCodec caps qrys req resp)
  -> Prepared route response
  -- ^ The route to encode
  -> Url
linkWith toMeta (Prepared route captures querys _) =
  encodePieces (metaPath m) (metaQuery m) captures querys
  where m = toMeta route

data Payload = Payload
  { payloadUrl :: !Url
  , payloadContent :: !(Maybe Content)
  , payloadAccepts :: !(NonEmpty N.MediaType)
  }

-- | Only useful for library authors
payloadWith
  :: forall route response
  .  (forall caps qrys req resp. route caps qrys req resp -> MetaClient caps qrys req resp)
  -> Prepared route (Clear response)
  -- ^ The route to be payload encoded
  -> Payload
payloadWith toMeta p@(Prepared route _ _ reqBody) =
  Payload url content accepts
  where
    url = linkWith toMeta p
    m = toMeta route
    content = encodeRequestBody (metaRequestBody m) reqBody
    accepts = case metaResponseBody m of
      ResponseBodyClear (Many decs) -> decs >>= \case
        BodyDecoding dec _ -> dec

-- Only useful to implement packages like 'trasa-client'
requestWith
  :: Functor m
  => (forall caps qrys req (resp :: Type). route caps qrys req (Clear resp) -> MetaClient caps qrys req (Clear resp))
  -> (Method -> Url -> Maybe Content -> NonEmpty N.MediaType -> m (Either TrasaErr Content))
  -- ^ method, url, content, accepts -> response
  -> Prepared route (Clear response)
  -> m (Either TrasaErr response)
requestWith toMeta run (Prepared route captures querys reqBody) =
  let m = toMeta route
      method = metaMethod m
      url = encodePieces (metaPath m) (metaQuery m) captures querys
      content = encodeRequestBody (metaRequestBody m) reqBody
      respBodyDecs = metaResponseBody m
      accepts = case respBodyDecs of
        ResponseBodyClear (Many decs) -> decs >>= \case
          BodyDecoding dec _ -> dec
   in fmap (\c -> c >>= decodeResponseBody respBodyDecs) (run method url content accepts)

encodeRequestBody :: RequestBody (Many BodyEncoding) request -> RequestBody Identity request -> Maybe Content
encodeRequestBody RequestBodyAbsent RequestBodyAbsent = Nothing
encodeRequestBody (RequestBodyPresent (Many encodings)) (RequestBodyPresent (Identity rq)) =
  case NE.head encodings of
    BodyEncoding names encoding -> Just (Content (NE.head names) (encoding rq))

decodeRequestBody
  :: RequestBody (Many BodyDecoding) req
  -> Maybe Content
  -> Either TrasaErr (RequestBody Identity req)
decodeRequestBody reqDec mcontent = case reqDec of
  RequestBodyPresent decs -> case mcontent of
    Nothing -> wrongBody
    Just (Content media bod) -> go (toList (getMany decs)) media bod
  RequestBodyAbsent -> case mcontent of
    Nothing -> Right RequestBodyAbsent
    Just _  -> wrongBody
  where
    wrongBody = Left (status N.status415)
    go :: [BodyDecoding a] -> N.MediaType -> LBS.ByteString -> Either TrasaErr (RequestBody Identity (Body a))
    go [] _ _ = Left (status N.status415)
    go (BodyDecoding medias dec:decs) media bod = case any (flip N.matches media) medias of
      True -> bimap (TrasaErr N.status415 . LBS.fromStrict . T.encodeUtf8)
                    (RequestBodyPresent . Identity)
                    (dec bod)
      False -> go decs media bod

encodeResponseBody
  :: forall response
  .  [N.MediaType]
  -> ResponseBody (Many BodyEncoding) (Clear response)
  -> response
  -> Either TrasaErr Content
encodeResponseBody medias (ResponseBodyClear encs) res = go (toList (getMany encs))
  where
    go :: [BodyEncoding response] -> Either TrasaErr Content
    go [] = Left (status N.status406)
    go (BodyEncoding accepts e:es) = case acceptable (toList accepts) medias of
      Just typ -> Right (Content typ (e res))
      Nothing  -> go es
    acceptable :: [N.MediaType] -> [N.MediaType] -> Maybe N.MediaType
    acceptable [] _ = Nothing
    acceptable (a:as) ms = case any (N.matches a) ms of
      True  -> Just a
      False -> acceptable as ms

decodeResponseBody :: ResponseBody (Many BodyDecoding) (Clear response) -> Content -> Either TrasaErr response
decodeResponseBody (ResponseBodyClear (Many decodings)) (Content name content) = go (toList decodings)
  where
    go :: [BodyDecoding response] -> Either TrasaErr response
    go [] = Left (status N.status415)
    go (BodyDecoding names dec:decs) = case any (N.matches name) names of
      True -> first (TrasaErr N.status400 . LBS.fromStrict . T.encodeUtf8) (dec content)
      False -> go decs

encodePieces
  :: Path CaptureEncoding captures
  -> Rec (Query CaptureEncoding) querys
  -> Rec Identity captures
  -> Rec Parameter querys
  -> Url
encodePieces pathEncoding queryEncoding path querys =
  Url (encodePath pathEncoding path) (QueryString (encodeQueries queryEncoding querys))
  where
    encodePath
      :: forall caps
      .  Path CaptureEncoding caps
      -> Rec Identity caps
      -> [T.Text]
    encodePath PathNil RNil = []
    encodePath (PathConsMatch str ps) xs = str : encodePath ps xs
    encodePath (PathConsCapture (CaptureEncoding enc) ps) (Identity x :& xs) = enc x : encodePath ps xs
    encodeQueries
      :: forall qrys
      .  Rec (Query CaptureEncoding) qrys
      -> Rec Parameter qrys
      -> HM.HashMap T.Text QueryParam
    encodeQueries RNil RNil = HM.empty
    encodeQueries (QueryFlag key :& encs) (ParameterFlag on :& qs) =
      if on then HM.insert key QueryParamFlag rest else rest
      where rest = encodeQueries encs qs
    encodeQueries (QueryOptional key (CaptureEncoding enc) :& encs) (ParameterOptional mval :& qs) =
      maybe rest (\val -> HM.insert key (QueryParamSingle (enc val)) rest) mval
      where rest = encodeQueries encs qs
    encodeQueries (QueryList key (CaptureEncoding enc) :& encs) (ParameterList vals :& qs) =
       HM.insert key (QueryParamList (fmap enc vals)) (encodeQueries encs qs)

-- | Build a router from all the possible routes, and methods to turn routes into needed metadata
routerWith
  :: forall route qryCodec reqCodec respCodec
  .  (forall caps qrys req resp. route caps qrys req resp -> Meta CaptureDecoding qryCodec reqCodec respCodec caps qrys req resp)
  -> [Constructed route]
  -> Router route
routerWith toMeta = Router . foldMap buildRouter
  where
    buildRouter :: Constructed route -> IxedRouter route Z
    buildRouter (Constructed route) = singletonIxedRouter route (metaMethod m) (metaPath m) (metaResponseBody m)
      where m = toMeta route


-- | Parses the path, the querystring, and the request body.
parseWith
  :: forall route capCodec respCodec
  .  (forall caps qrys req resp. route caps qrys req resp -> Meta capCodec CaptureDecoding (Many BodyDecoding) respCodec caps qrys req resp)
  -> Router route -- ^ Router
  -> Method -- ^ Request Method
  -> Url -- ^ Everything after the authority
  -> Maybe Content -- ^ Request content type and body
  -> Either TrasaErr (Concealed route)
parseWith toMeta madeRouter method (Url encodedPath encodedQuery) mcontent = do
  -- Currently, we are silently discarding path pieces for raw
  -- routes. Reconsider this at some future time because it is not
  -- actually that hard to support this.
  Pathed route captures _ <- maybe (Left (status N.status404)) Right
    $ parsePathWith madeRouter method encodedPath
  let m = toMeta route
  querys <- parseQueryWith (metaQuery m) encodedQuery
  reqBody <- decodeRequestBody (metaRequestBody m) mcontent
  return (Concealed route captures querys reqBody)

-- | Parses only the path.
parsePathWith :: forall route.
     Router route
  -> Method -- ^ Method
  -> [T.Text] -- ^ Path Pieces
  -> Maybe (Pathed route)
parsePathWith (Router r0) method pieces0 =
  listToMaybe (go VecNil pieces0 r0)
  where
  go :: forall n.
        Vec n T.Text -- captures being accumulated
     -> [T.Text] -- remaining pieces
     -> IxedRouter route n -- router fragment
     -> [Pathed route]
  go captures ps (IxedRouter matches mcapture responders rawResponders) =
    let encMethod = encodeMethod method in
    case HM.lookup encMethod rawResponders of
      Just rawRespondersAtMethod -> mapMaybe
        (\(IxedRawResponder route capDecs) -> 
          fmap (\x -> Pathed route x (ExtraPathRaw ps)) (decodeCaptureVector capDecs captures)
        ) 
        rawRespondersAtMethod
      Nothing -> case ps of
        [] -> case HM.lookup encMethod responders of
          Nothing -> []
          Just respondersAtMethod ->
            mapMaybe (\(IxedClearResponder route capDecs) ->
              fmap (\x -> (Pathed route x ExtraPathClear)) (decodeCaptureVector capDecs captures)
            ) respondersAtMethod
        p : psNext ->
          let res1 = maybe [] id $ fmap (go captures psNext) (HM.lookup p matches)
              -- Since this uses snocVec to build up the captures,
              -- this algorithm's complexity includes a term that is
              -- O(n^2) in the number of captures. However, most routes
              -- that I deal with have one or two captures. Occassionally,
              -- I'll get one with four or five, but this happens
              -- so infrequently that I'm not concerned about this.
              res2 = maybe [] id $ fmap (go (snocVec p captures) psNext) mcapture
           in res1 ++ res2

parseQueryWith :: Rec (Query CaptureDecoding) querys -> QueryString -> Either TrasaErr (Rec Parameter querys)
parseQueryWith decoding (QueryString querys) = rtraverse param decoding
  where
    param :: forall qry. Query CaptureDecoding qry -> Either TrasaErr (Parameter qry)
    param = \case
      QueryFlag key -> Right (ParameterFlag (HM.member key querys))
      QueryOptional key (CaptureDecoding dec) -> case HM.lookup key querys of
        Nothing -> Right (ParameterOptional Nothing)
        Just query -> case query of
          QueryParamFlag -> Left (TrasaErr N.status400 "query flag given when key-value expected")
          QueryParamSingle txt -> Right (ParameterOptional (dec txt))
          QueryParamList _ -> Left (TrasaErr N.status400 "query param list given when key-value expected")
      QueryList key (CaptureDecoding dec) -> case HM.lookup key querys of
        Nothing -> Right (ParameterList [])
        Just query -> case query of
          QueryParamFlag -> Left (TrasaErr N.status400 "query flag given when list expected")
          QueryParamSingle txt -> Right (ParameterList (maybe [] (:[]) (dec txt)))
          QueryParamList txts -> Right (ParameterList (mapMaybe dec txts))

decodeCaptureVector ::
     IxedRec CaptureDecoding n xs
  -> Vec n T.Text
  -> Maybe (Rec Identity xs)
decodeCaptureVector IxedRecNil VecNil = Just RNil
decodeCaptureVector (IxedRecCons (CaptureDecoding decode) rnext) (VecCons piece vnext) = do
  val <- decode piece
  vals <- decodeCaptureVector rnext vnext
  return (Identity val :& vals)

type family ParamBase (param :: Param) :: Type where
  ParamBase Flag = Bool
  ParamBase (Optional a) = Maybe a
  ParamBase (List a) = [a]

demoteParameter :: Parameter param -> ParamBase param
demoteParameter = \case
  ParameterFlag b -> b
  ParameterOptional m -> m
  ParameterList l -> l

-- | A closed, total type family provided as a convenience to end users.
--   Other function is this library take advantage of 'Arguments' to allow
--   end users use normal function application. Without this, users would
--   need to write out 'Record' and 'RequestBody' values by hand, which
--   is tedious.
--
--   >>> :kind! Arguments '[Int,Bool] '[Flag,Optional Double,List Int] 'Bodyless Double
--   Arguments '[Int,Bool] '[Flag,Optional Double,List Int] 'Bodyless Double :: *
--   = Int -> Bool -> Bool -> Maybe Double -> [Int] -> Double
type family Arguments (pieces :: [Type]) (querys :: [Param]) (body :: Bodiedness) (result :: Type) :: Type where
  Arguments '[] '[] ('Body b) r = b -> r
  Arguments '[] '[] 'Bodyless r = r
  Arguments '[] (q ': qs) r b = ParamBase q -> Arguments '[] qs r b
  Arguments (c ': cs) qs b r = c -> Arguments cs qs b r


-- | Used my users to define a function called prepare, see tutorial
prepareWith
  :: (forall caps qrys req resp. route caps qrys req resp -> Meta capCodec qryCodec reqCodec respCodec caps qrys req resp)
  -> route captures query request response
  -- ^ The route to prepare
  -> Arguments captures query request (Prepared route response)
prepareWith toMeta route =
  prepareExplicit route (metaPath m) (metaQuery m) (metaRequestBody m)
  where m = toMeta route

prepareExplicit :: forall route captures queries request response rqf pf qf.
     route captures queries request response
  -> Path pf captures
  -> Rec (Query qf) queries
  -> RequestBody rqf request
  -> Arguments captures queries request (Prepared route response)
prepareExplicit route = go (Prepared route)
  where
  -- Adopted from: https://www.reddit.com/r/haskell/comments/67l9so/currying_a_typelevel_list/dgrghxz/
  go :: forall caps qrys z.
        (Rec Identity caps -> Rec Parameter qrys -> RequestBody Identity request -> z)
     -> Path pf caps
     -> Rec (Query qf) qrys
     -> RequestBody rqf request
     -> Arguments caps qrys request z
  go k PathNil RNil RequestBodyAbsent =
    k RNil RNil RequestBodyAbsent
  go k PathNil RNil (RequestBodyPresent _) =
    \reqBod -> k RNil RNil (RequestBodyPresent (Identity reqBod))
  go k PathNil (q :& qs) b =
    \qt -> go (\caps querys reqBody -> k caps (parameter q qt :& querys) reqBody) PathNil qs b
  go k (PathConsMatch _ pnext) qs b =
    go k pnext qs b
  go k (PathConsCapture _ pnext) qs b =
    \c -> go (\caps querys reqBod -> k (Identity c :& caps) querys reqBod) pnext qs b
  parameter :: forall param. Query qf param -> ParamBase param -> Parameter param
  parameter (QueryFlag _) b = ParameterFlag b
  parameter (QueryOptional _ _) m = ParameterOptional m
  parameter (QueryList _ _) l = ParameterList l

-- | Uncurry the arguments type family
handler :: forall captures querys request x.
     Rec Identity captures
  -> Rec Parameter querys
  -> RequestBody Identity request
  -> Arguments captures querys request x
  -> x
handler = go
  where
  go :: forall caps qrys.
       Rec Identity caps
    -> Rec Parameter qrys
    -> RequestBody Identity request
    -> Arguments caps qrys request x
    -> x
  go RNil RNil RequestBodyAbsent f = f
  go RNil RNil (RequestBodyPresent (Identity b)) f = f b
  go RNil (q :& qs) b f = go RNil qs b (f (demoteParameter q))
  go (Identity c :& cs) qs b f = go cs qs b (f c)

-- | A route with all types hidden: the captures, the request body,
--   and the response body. This is needed so that users can
--   enumerate over all the routes.
data Constructed :: ([Type] -> [Param] -> Bodiedness -> Clarity r -> Type) -> Type where
  Constructed :: !(route captures querys request response) -> Constructed route
-- I dont really like the name Constructed, but I don't want to call it
-- Some or Any since these get used a lot and a conflict would be likely.
-- Think, think, think.

mapConstructed ::
     (forall caps qrys req resp. sub caps qrys req resp -> route caps qrys req resp)
  -> Constructed sub
  -> Constructed route
mapConstructed f (Constructed sub) = Constructed (f sub)

data Pathed :: ([Type] -> [Param] -> Bodiedness -> Clarity r -> Type) -> Type  where
  Pathed :: !(route captures querys request response) -> !(Rec Identity captures) -> ExtraPath response -> Pathed route

data ExtraPath :: Clarity r -> Type where
  ExtraPathRaw :: [T.Text] -> ExtraPath 'Raw
  ExtraPathClear :: forall (a :: Type). ExtraPath ('Clear a)

-- | Includes the route, path, query parameters, and request body.
data Prepared :: ([Type] -> [Param] -> Bodiedness -> Clarity r -> Type) -> Clarity r -> Type where
  Prepared ::
       !(route captures querys request response)
    -> !(Rec Identity captures)
    -> !(Rec Parameter querys)
    -> !(RequestBody Identity request)
    -> Prepared route response

-- | Only needed to implement 'parseWith'. Most users do not need this.
--   If you need to create a route hierarchy to provide breadcrumbs,
--   then you will need this.
data Concealed :: ([Type] -> [Param] -> Bodiedness -> Clarity r -> Type) -> Type where
  Concealed ::
       !(route captures querys request response)
    -> !(Rec Identity captures)
    -> !(Rec Parameter querys)
    -> !(RequestBody Identity request)
    -> Concealed route

-- | Conceal the response type.
conceal :: Prepared route response -> Concealed route
conceal (Prepared route caps querys req) = Concealed route caps querys req

concealedToPrepared
  :: forall route a
  .  Concealed route
  -> (forall resp. Prepared route resp -> a)
  -> a
concealedToPrepared (Concealed route caps qrys req) f = f (Prepared route caps qrys req)

-- | The HTTP content type and body.
data Content = Content
  { contentType :: !N.MediaType
  , contentData :: !LBS.ByteString
  } deriving (Show,Eq,Ord)

-- | Only promoted version used.
data Nat = S !Nat | Z

newtype Router route = Router (IxedRouter route 'Z)

data IxedRouter (f :: [Type] -> [Param] -> Bodiedness -> Clarity r -> Type) (n :: Nat) where
  IxedRouter :: 
       !(HashMap T.Text (IxedRouter route n))
       -- All possible static matches
    -> !(Maybe (IxedRouter route ('S n)))
       -- A capture
    -> !(HashMap T.Text [IxedClearResponder route n])
       -- This is a map from HTTP request methods to responders.
       -- There should be either zero or one elements in each 
       -- list, more than one means that there are trivially overlapped routes.
    -> !(HashMap T.Text [IxedRawResponder route n])
       -- Request methods that result in a raw application.
       -- This is usually empty.
    -> IxedRouter route n

-- | This monoid instance is provided so that we can
--   conveniently use foldMap elsewhere. We do not
--   provide a Monoid instance for Router like we do
--   for IxedRouter. End users only have one way to create
--   a router, and if they combine a Router with itself
--   using mappend, it would result in Router in which all
--   routes were overlapped. Just to be clear, this
--   instance is completely lawful.
instance Monoid (IxedRouter route n) where
  mempty = IxedRouter HM.empty Nothing HM.empty HM.empty
  mappend = unionIxedRouter

data IxedClearResponder :: ([Type] -> [Param] -> Bodiedness -> Clarity r -> Type) -> Nat -> Type where
  IxedClearResponder :: forall route captures query request n (a :: Type).
       !(route captures query request (Clear a))
    -> !(IxedRec CaptureDecoding n captures)
    -> IxedClearResponder route n

data IxedRawResponder :: ([Type] -> [Param] -> Bodiedness -> Clarity r -> Type) -> Nat -> Type where
  IxedRawResponder :: forall route captures query request n.
       !(route captures query request Raw)
    -> !(IxedRec CaptureDecoding n captures)
    -> IxedRawResponder route n

data IxedRec :: (k -> Type) -> Nat -> [k] -> Type where
  IxedRecNil :: IxedRec f 'Z '[]
  IxedRecCons :: !(f r) -> !(IxedRec f n rs) -> IxedRec f ('S n) (r ': rs)

data Vec :: Nat -> Type -> Type where
  VecNil :: Vec 'Z a
  VecCons :: !a -> !(Vec n a) -> Vec ('S n) a

data IxedPath :: (Type -> Type) -> Nat -> [Type] -> Type where
  IxedPathNil :: IxedPath f 'Z '[]
  IxedPathCapture :: !(f a) -> !(IxedPath f n as) -> IxedPath f ('S n) (a ': as)
  IxedPathMatch :: !T.Text -> !(IxedPath f n a) -> IxedPath f n a

data LenPath :: Nat -> Type where
  LenPathNil :: LenPath 'Z
  LenPathCapture :: !(LenPath n) -> LenPath ('S n)
  LenPathMatch :: !T.Text -> !(LenPath n) -> LenPath n

snocVec :: a -> Vec n a -> Vec ('S n) a
snocVec a VecNil = VecCons a VecNil
snocVec a (VecCons b vnext) =
  VecCons b (snocVec a vnext)

type family Length (xs :: [k]) :: Nat where
  Length '[] = Z
  Length (x ': xs) = S (Length xs)

pathToIxedPath :: Path f xs -> IxedPath f (Length xs) xs
pathToIxedPath PathNil = IxedPathNil
pathToIxedPath (PathConsCapture c pnext) = IxedPathCapture c (pathToIxedPath pnext)
pathToIxedPath (PathConsMatch s pnext) = IxedPathMatch s (pathToIxedPath pnext)

-- | Discards the static parts
ixedPathToIxedRec :: IxedPath f n xs -> IxedRec f n xs
ixedPathToIxedRec IxedPathNil = IxedRecNil
ixedPathToIxedRec (IxedPathCapture c pnext) =
  IxedRecCons c (ixedPathToIxedRec pnext)
ixedPathToIxedRec (IxedPathMatch _ pnext) =
  ixedPathToIxedRec pnext

ixedPathToLenPath :: IxedPath f n xs -> LenPath n
ixedPathToLenPath IxedPathNil = LenPathNil
ixedPathToLenPath (IxedPathCapture _ pnext) =
  LenPathCapture (ixedPathToLenPath pnext)
ixedPathToLenPath (IxedPathMatch s pnext) =
  LenPathMatch s (ixedPathToLenPath pnext)

type family (n :: Nat) + (m :: Nat) :: Nat where
  Z + m = m
  (S n) + m = S (n + m)

plusRightIdentity :: forall n. n :~: n + Z
plusRightIdentity = unsafeCoerce Refl

plusRightSucc :: forall n m. S (n + m) :~: n + S m
plusRightSucc = unsafeCoerce Refl

coerce :: a :~: b -> f a -> f b
coerce Refl a = a

reverseLenPathMatch :: LenPath n -> LenPath n
reverseLenPathMatch = go LenPathNil
  where
  go :: forall n m. LenPath n -> LenPath m -> LenPath (n + m)
  go acc LenPathNil = coerce (plusRightIdentity @n) acc
  go acc (LenPathMatch s pnext) = go (LenPathMatch s acc) pnext
  go acc (LenPathCapture (pnext :: LenPath m2)) = coerce (plusRightSucc @n @m2) (go (LenPathCapture acc) pnext)

singletonIxedRouter
  :: forall
     (route :: [Type] -> [Param] -> Bodiedness -> Clarity r -> Type)
     rpf captures request querys (response :: Clarity r)
   . route captures querys request response
  -> Method
  -> Path CaptureDecoding captures
  -> ResponseBody rpf response
  -> IxedRouter route 'Z
singletonIxedRouter route method capDecs respBody =
    let ixedCapDecs = pathToIxedPath capDecs
        ixedCapDecsRec = ixedPathToIxedRec ixedCapDecs
        lenPath = reverseLenPathMatch (ixedPathToLenPath ixedCapDecs)
     in case respBody of
          ResponseBodyClear _ -> singletonClearIxedRouterHelper (IxedClearResponder route ixedCapDecsRec) method lenPath
          ResponseBodyRaw _ -> singletonRawIxedRouterHelper (IxedRawResponder route ixedCapDecsRec) method lenPath

singletonRawIxedRouterHelper
  :: forall
     (n :: Nat)
     (route :: [Type] -> [Param] -> Bodiedness -> Clarity r -> Type)
   . IxedRawResponder route n
  -> Method
  -> LenPath n
  -> IxedRouter route 'Z
singletonRawIxedRouterHelper responder method path =
  let r = IxedRouter HM.empty Nothing HM.empty (HM.singleton (encodeMethod method) [responder])
   in singletonIxedRouterGo r path

singletonClearIxedRouterHelper
  :: forall
     (n :: Nat)
     (route :: [Type] -> [Param] -> Bodiedness -> Clarity r -> Type)
   . IxedClearResponder route n
  -> Method
  -> LenPath n
  -> IxedRouter route 'Z
singletonClearIxedRouterHelper responder method path =
  let r = IxedRouter HM.empty Nothing (HM.singleton (encodeMethod method) [responder]) HM.empty
   in singletonIxedRouterGo r path

singletonIxedRouterGo ::
  IxedRouter route n -> LenPath n -> IxedRouter route 'Z
singletonIxedRouterGo r lp = case lp of
  LenPathNil -> r
  LenPathCapture lpNext -> singletonIxedRouterGo (IxedRouter HM.empty (Just r) HM.empty HM.empty) lpNext
  LenPathMatch s lpNext -> singletonIxedRouterGo (IxedRouter (HM.singleton s r) Nothing HM.empty HM.empty) lpNext

unionIxedRouter :: IxedRouter route n -> IxedRouter route n -> IxedRouter route n
unionIxedRouter = go
  where
  go :: forall route n. IxedRouter route n -> IxedRouter route n -> IxedRouter route n
  go (IxedRouter matchesA captureA respsA rawRespsA) (IxedRouter matchesB captureB respsB rawRespsB) =
    IxedRouter
      (HM.unionWith go matchesA matchesB)
      (unionMaybeWith go captureA captureB)
      (HM.unionWith (++) respsA respsB)
      (HM.unionWith (++) rawRespsA rawRespsB)

unionMaybeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybeWith f x y = case x of
  Nothing -> y
  Just xval -> case y of
    Nothing -> x
    Just yval -> Just (f xval yval)

-- | Pretty prints a router, using indentation to show nesting
--   of routes under a common prefix. This also shows the request
--   methods that each route accepts. If there are any trivially
--   overlapped routes, the appends are asterisk to the method name
--   for which the routes are overlapped.
prettyRouter :: Router route -> String
prettyRouter (Router r) = L.unlines (prettyIxedRouter 0 (Nothing,r))

prettyIxedRouter ::
     Int -- ^ Indentation
  -> (Maybe String, IxedRouter route n)
  -> [String]
prettyIxedRouter indent (mnode,IxedRouter matches cap responders rawResponders) =
  let spaces = L.replicate indent ' '
      nextIndent = if isJust mnode then indent + 2 else indent
      children1 = map (first (Just . ('/' : ) . T.unpack)) (HM.toList matches)
      children2 = maybe [] (\x -> [(Just "/:capture",x)]) cap
      combinedResponders = HM.unionWith (++) ((fmap.fmap) (const ()) responders) ((fmap.fmap) (const ()) rawResponders)
   in concat
        [ case mnode of
            Nothing -> if length combinedResponders > 0
              then ["/ " ++ showRespondersList combinedResponders]
              else []
            Just _ -> []
        , maybe [] (\x -> [x]) $ flip fmap mnode $ \node -> spaces
            ++ node
            ++ (if length combinedResponders > 0 then " " ++ showRespondersList combinedResponders else "")
        , prettyIxedRouter nextIndent =<< children1
        , prettyIxedRouter nextIndent =<< children2
        ]

showRespondersList :: HashMap T.Text [()] -> String
showRespondersList = id
  . (\x -> "[" ++ x ++ "]")
  . L.intercalate ","
  . map (\(method,xs) -> T.unpack method ++ (if L.length xs > 1 then "*" else ""))
  . HM.toList
