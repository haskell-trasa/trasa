{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-unticked-promoted-constructors #-}
module Trasa.Core
  (
  -- * Types
    Bodiedness(..)
  , Path(..)
  , ResponseBody(..)
  , RequestBody(..)
  , Param(..)
  , Query(..)
  , Parameter(..)
  , BodyCodec(..)
  , BodyDecoding(..)
  , BodyEncoding(..)
  , Many(..)
  , CaptureCodec(..)
  , CaptureEncoding(..)
  , CaptureDecoding(..)
  , Content(..)
  , QueryString(..)
  , Url(..)
  , Payload(..)
  , TrasaErr(..)
  , Router
  -- ** Existential
  , Prepared(..)
  , Concealed(..)
  , Constructed(..)
  -- * Url
  , encodeUrl
  , decodeQuery
  , decodeUrl
  -- * Using Routes
  , prepareWith
  , dispatchWith
  , parseWith
  , linkWith
  , payloadWith
  , requestWith
  , routerWith
  , handler
  -- * Defining Routes
  -- ** Path
  , match
  , capture
  , end
  , (./)
  , appendPath
  -- ** Request Body
  , body
  , bodyless
  -- ** Response Body
  , resp
  -- ** Query
  , demoteParameter
  , flag
  , optional
  , list
  , qend
  , (.&)
  , mapQuerys
  -- * Converting Route Metadata
  , mapPath
  , mapMany
  , mapRequestBody
  , mapResponseBody
  , mapConstructed
  -- * Converting Codecs
  , bodyCodecToBodyEncoding
  , bodyCodecToBodyDecoding
  , captureCodecToCaptureEncoding
  , captureCodecToCaptureDecoding
  -- * Errors
  , status
  -- * Argument Currying
  , ParamBase
  , Arguments
  -- * Random Stuff
  , conceal
  , encodeRequestBody
  , decodeResponseBody
  , prettyRouter
  -- * Show/Read Codecs
  , showReadBodyCodec
  , showReadCaptureCodec
  ) where

import Data.Kind (Type)
import Data.Functor.Identity (Identity(..))
import Control.Applicative (liftA2)
import Data.Maybe (mapMaybe,listToMaybe,isJust)
import Data.Semigroup (Semigroup(..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Foldable (toList)
import Data.Bifunctor (first)
import Text.Read (readEither,readMaybe)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Binary.Builder as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as N
import qualified Network.HTTP.Types.URI as N
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Vinyl (Rec(..),rmap)
import Data.Vinyl.TypeLevel (type (++))

-- $setup
-- >>> :set -XTypeInType

data Bodiedness = forall a. Body a | Bodyless

data RequestBody :: (Type -> Type) -> Bodiedness -> Type where
  RequestBodyPresent :: f a -> RequestBody f ('Body a)
  RequestBodyAbsent :: RequestBody f 'Bodyless

mapRequestBody :: (forall x. rqf x -> rqf' x) -> RequestBody rqf request -> RequestBody rqf' request
mapRequestBody _ RequestBodyAbsent = RequestBodyAbsent
mapRequestBody f (RequestBodyPresent reqBod) = RequestBodyPresent (f reqBod)

newtype ResponseBody rpf response = ResponseBody { getResponseBody :: rpf response }

mapResponseBody :: (forall x. rpf x -> rpf' x) -> ResponseBody rpf request -> ResponseBody rpf' request
mapResponseBody f (ResponseBody resBod) = ResponseBody (f resBod)

data Path :: (Type -> Type) -> [Type] -> Type where
  PathNil :: Path cap '[]
  PathConsCapture :: cap a -> Path cap as -> Path cap (a ': as)
  PathConsMatch :: T.Text -> Path cap as -> Path cap as

mapPath :: (forall x. cf x -> cf' x) -> Path cf ps -> Path cf' ps
mapPath _ PathNil = PathNil
mapPath f (PathConsMatch s pnext) = PathConsMatch s (mapPath f pnext)
mapPath f (PathConsCapture c pnext) = PathConsCapture (f c) (mapPath f pnext)

appendPath :: Path f as -> Path f bs -> Path f (as ++ bs)
appendPath PathNil bs = bs
appendPath (PathConsMatch a as) bs = PathConsMatch a (appendPath as bs)
appendPath (PathConsCapture cas as) bs = PathConsCapture cas (appendPath as bs)

newtype Many f a = Many { getMany :: NonEmpty (f a) }
  deriving (Functor)

instance Applicative f => Applicative (Many f) where
  pure = Many . pure . pure
  Many mf <*> Many mx = Many $ liftA2 (<*>) mf mx

mapMany :: (forall x. f x -> g x) -> Many f a -> Many g a
mapMany eta (Many m) = Many (fmap eta m)

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

data Param
  = Flag
  | forall a. Optional a
  | forall a. List a

data Query :: (Type -> Type) -> Param -> Type where
  QueryFlag :: T.Text -> Query cap Flag
  QueryOptional :: T.Text -> cap a -> Query cap (Optional a)
  QueryList :: T.Text -> cap a -> Query cap (List a)

data Parameter :: Param -> Type where
  ParameterFlag :: Bool -> Parameter Flag
  ParameterOptional :: Maybe a -> Parameter (Optional a)
  ParameterList :: [a] -> Parameter (List a)

data QueryParam
  = QueryParamFlag
  | QueryParamSingle T.Text
  | QueryParamList [T.Text]
  deriving Eq

instance Semigroup QueryParam where
  QueryParamFlag <> q = q
  q <> QueryParamFlag = q
  QueryParamSingle q1 <> QueryParamSingle q2 = QueryParamList [q1,q2]
  QueryParamSingle q1 <> QueryParamList l1 = QueryParamList (q1:l1)
  QueryParamList l1 <> QueryParamSingle q1 = QueryParamList (l1 ++ [q1]) -- Change list to a set
  QueryParamList l1 <> QueryParamList l2 = QueryParamList (l1 ++ l2)

newtype QueryString = QueryString
  { unQueryString :: HM.HashMap T.Text QueryParam
  } deriving Eq

infixr 7 ./

(./) :: (a -> b) -> a -> b
(./) f a = f a

match :: T.Text -> Path cpf caps -> Path cpf caps
match = PathConsMatch

capture :: cpf cap -> Path cpf caps -> Path cpf (cap ': caps)
capture = PathConsCapture

end :: Path cpf '[]
end = PathNil

body :: rqf req -> RequestBody rqf ('Body req)
body = RequestBodyPresent

bodyless :: RequestBody rqf 'Bodyless
bodyless = RequestBodyAbsent

resp :: rpf resp -> ResponseBody rpf resp
resp = ResponseBody

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

mapQuerys :: (forall x. f x -> g x) -> Rec (Query f) qs -> Rec (Query g) qs
mapQuerys eta = rmap $ \case
  QueryFlag key -> QueryFlag key
  QueryOptional key query -> QueryOptional key (eta query)
  QueryList key query -> QueryList key (eta query)

data CaptureCodec a = CaptureCodec
  { captureCodecEncode :: a -> T.Text
  , captureCodecDecode :: T.Text -> Maybe a
  }

newtype CaptureEncoding a = CaptureEncoding { appCaptureEncoding :: a -> T.Text }
newtype CaptureDecoding a = CaptureDecoding { appCaptureDecoding :: T.Text -> Maybe a }

captureCodecToCaptureEncoding :: CaptureCodec a -> CaptureEncoding a
captureCodecToCaptureEncoding (CaptureCodec enc _) = CaptureEncoding enc

captureCodecToCaptureDecoding :: CaptureCodec a -> CaptureDecoding a
captureCodecToCaptureDecoding (CaptureCodec _ dec) = CaptureDecoding dec

data Url = Url
  { urlPath :: ![T.Text]
  , urlQueryString :: !QueryString
  } deriving Eq

instance Show Url where
  show = show . encodeUrl

encodeUrl :: Url -> T.Text
encodeUrl (Url path (QueryString querys)) =
  ( T.decodeUtf8
  . LBS.toStrict
  . LBS.toLazyByteString
  . encode
  . HM.foldrWithKey (\key param items -> toQueryItem key param ++ items) []) querys
  where
    encode qs = case path of
      [] -> "/" <> N.encodePath path qs
      _  -> N.encodePath path qs
    toQueryItem :: T.Text -> QueryParam -> [N.QueryItem]
    toQueryItem key = \case
      QueryParamFlag -> [(T.encodeUtf8 key, Nothing)]
      QueryParamSingle value -> [(T.encodeUtf8 key, Just (T.encodeUtf8 value))]
      QueryParamList values ->
        flip fmap values $ \value -> (T.encodeUtf8 key, Just (T.encodeUtf8 value))

decodeQuery :: N.Query -> QueryString
decodeQuery = QueryString . HM.fromListWith (<>) . fmap decode
  where
    decode (key,mval) = case mval of
      Nothing  -> (tkey,QueryParamFlag)
      Just val -> (tkey,QueryParamSingle (T.decodeUtf8 val))
      where tkey = T.decodeUtf8 key

decodeUrl :: T.Text -> Url
decodeUrl txt = Url path (decodeQuery querys)
  where (path,querys) = N.decodePath (T.encodeUtf8 txt)

linkWith :: forall route response.
     (forall caps qrys req resp. route caps qrys req resp -> Path CaptureEncoding caps)
  -> (forall caps qrys req resp. route caps qrys req resp -> Rec (Query CaptureEncoding) qrys)
  -> Prepared route response
  -> Url
linkWith toCapEncs toQuerys (Prepared route captures querys _) =
  encodePieces (toCapEncs route) (toQuerys route) captures querys
-- We should probably go ahead and just URL encode the path
-- when someone calls linkWith.

data Payload = Payload
  { payloadUrl :: !Url
  , payloadContent :: !(Maybe Content)
  , payloadAccepts :: !(NonEmpty T.Text)
  }

payloadWith :: forall route response.
     (forall caps qrys req resp. route caps qrys req resp -> Path CaptureEncoding caps)
  -> (forall caps qrys req resp. route caps qrys req resp -> Rec (Query CaptureEncoding) qrys)
  -> (forall caps qrys req resp. route caps qrys req resp -> RequestBody (Many BodyEncoding) req)
  -> (forall caps qrys req resp. route caps qrys req resp -> ResponseBody (Many BodyDecoding) resp)
  -> Prepared route response
  -> Payload
payloadWith toCapEncs toQuerys toReqBody toRespBody p@(Prepared route _ _ reqBody) =
  Payload url content accepts
  where
    url = linkWith toCapEncs toQuerys p
    content = encodeRequestBody (toReqBody route) reqBody
    ResponseBody (Many decodings) = toRespBody route
    accepts = bodyDecodingNames =<< decodings

requestWith :: Functor m
  => (forall caps querys req resp. route caps querys req resp -> T.Text)
  -> (forall caps querys req resp. route caps querys req resp -> Path CaptureEncoding caps)
  -> (forall caps querys req resp. route caps querys req resp -> Rec (Query CaptureEncoding) querys)
  -> (forall caps querys req resp. route caps querys req resp -> RequestBody (Many BodyEncoding) req)
  -> (forall caps querys req resp. route caps querys req resp -> ResponseBody (Many BodyDecoding) resp)
  -> (T.Text -> Url -> Maybe Content -> [T.Text] -> m Content)
     -- ^ method, url, content, accepts -> response
  -> Prepared route response
  -> m (Maybe response)
requestWith toMethod toCapEncs toQuerys toReqBody toRespBody run (Prepared route captures querys reqBody) =
  let method = toMethod route
      url = encodePieces (toCapEncs route) (toQuerys route) captures querys
      content = encodeRequestBody (toReqBody route) reqBody
      respBodyDecs = toRespBody route
      ResponseBody (Many decodings) = respBodyDecs
      accepts = toList (bodyDecodingNames =<< decodings)
   in fmap (decodeResponseBody respBodyDecs) (run method url content accepts)

encodeRequestBody :: RequestBody (Many BodyEncoding) request -> RequestBody Identity request -> Maybe Content
encodeRequestBody RequestBodyAbsent RequestBodyAbsent = Nothing
encodeRequestBody (RequestBodyPresent (Many encodings)) (RequestBodyPresent (Identity rq)) =
  case NE.head encodings of
    BodyEncoding names encoding -> Just (Content (NE.head names) (encoding rq))

decodeResponseBody :: ResponseBody (Many BodyDecoding) response -> Content -> Maybe response
decodeResponseBody (ResponseBody (Many decodings)) (Content name content) =
  flip mapFind decodings $ \(BodyDecoding names decode) ->
    if elem name names then hush (decode content) else Nothing
  where hush (Left _)  = Nothing
        hush (Right a) = Just a

encodePieces
  :: Path CaptureEncoding captures
  -> Rec (Query CaptureEncoding) querys
  -> Rec Identity captures
  -> Rec Parameter querys
  -> Url
encodePieces pathEncoding queryEncoding path querys =
  Url (encodePath pathEncoding path) (QueryString (encodeQuerys queryEncoding querys))
  where
    encodePath
      :: forall caps
      .  Path CaptureEncoding caps
      -> Rec Identity caps
      -> [T.Text]
    encodePath PathNil RNil = []
    encodePath (PathConsMatch str ps) xs = str : encodePath ps xs
    encodePath (PathConsCapture (CaptureEncoding enc) ps) (Identity x :& xs) = enc x : encodePath ps xs
    encodeQuerys
      :: forall qrys
      .  Rec (Query CaptureEncoding) qrys
      -> Rec Parameter qrys
      -> HM.HashMap T.Text QueryParam
    encodeQuerys RNil RNil = HM.empty
    encodeQuerys (QueryFlag key :& encs) (ParameterFlag on :& qs) =
      if on then HM.insert key QueryParamFlag rest else rest
      where rest = encodeQuerys encs qs
    encodeQuerys (QueryOptional key (CaptureEncoding enc) :& encs) (ParameterOptional mval :& qs) =
      maybe rest (\val -> HM.insert key (QueryParamSingle (enc val)) rest) mval
      where rest = encodeQuerys encs qs
    encodeQuerys (QueryList key (CaptureEncoding enc) :& encs) (ParameterList vals :& qs) =
       HM.insert key (QueryParamList (fmap enc vals)) (encodeQuerys encs qs)

data TrasaErr = TrasaErr
  { trasaErrStatus :: N.Status
  , trasaErrBody :: LBS.ByteString
  } deriving (Show,Eq,Ord)

status :: N.Status -> TrasaErr
status s = TrasaErr s ""

dispatchWith :: forall route m.
     Applicative m
  => (forall caps qrys req resp. route caps qrys req resp -> Rec (Query CaptureDecoding) qrys)
  -> (forall caps qrys req resp. route caps qrys req resp -> RequestBody (Many BodyDecoding) req)
  -> (forall caps qrys req resp. route caps qrys req resp -> ResponseBody (Many BodyEncoding) resp)
  -> (forall caps qrys req resp. route caps qrys req resp -> Rec Identity caps -> Rec Parameter qrys -> RequestBody Identity req -> m resp)
  -> Router route -- ^ Router
  -> T.Text -- ^ Method
  -> [T.Text] -- ^ Accept headers
  -> Url -- ^ Everything after the authority
  -> Maybe Content -- ^ Content type and request body
  -> m (Either TrasaErr Content) -- ^ Encoded response
dispatchWith toQuerys toReqBody toRespBody makeResponse router method accepts url mcontent =
  sequenceA $ do
    Concealed route decodedPathPieces decodedQuerys decodedRequestBody <-
      parseWith toQuerys toReqBody router method url mcontent
    let response = makeResponse route decodedPathPieces decodedQuerys decodedRequestBody
        ResponseBody (Many encodings) = toRespBody route
    (encode,typ) <- mapFindE (status N.status406)
      (\(BodyEncoding names encode) -> case mapFind (\x -> if elem x accepts then Just x else Nothing) names of
        Just name -> Just (encode,name)
        Nothing -> Nothing
      )
      encodings
    Right (fmap (Content typ . encode) response)

routerWith ::
     (forall caps querys req resp. route caps querys req resp -> T.Text)
  -> (forall caps querys req resp. route caps querys req resp -> Path CaptureDecoding caps)
  -> [Constructed route]
  -> Router route
routerWith toMethod toCapDec enumeratedRoutes = Router $ foldMap
  (\(Constructed route) -> singletonIxedRouter route (toMethod route) (toCapDec route))
  enumeratedRoutes

-- | Parses the path, the querystring (once this gets added), and
--   the request body.
parseWith :: forall route.
     (forall caps qrys req resp. route caps qrys req resp -> Rec (Query CaptureDecoding) qrys)
  -> (forall caps qrys req resp. route caps qrys req resp -> RequestBody (Many BodyDecoding) req)
  -> Router route -- ^ Router
  -> T.Text -- ^ Request Method
  -> Url -- ^ Everything after the authority
  -> Maybe Content -- ^ Request content type and body
  -> Either TrasaErr (Concealed route)
parseWith toQuerys toReqBody router method (Url encodedPath encodedQuery) mcontent = do
  Pathed route captures <- maybe (Left (status N.status404)) Right
    $ parsePathWith router method encodedPath
  querys <- parseQueryWith (toQuerys route) encodedQuery
  decodedRequestBody <- case toReqBody route of
    RequestBodyPresent (Many decodings) -> case mcontent of
      Just (Content typ encodedRequest) -> do
        decode <- mapFindE (status N.status415) (\(BodyDecoding names decode) -> if elem typ names then Just decode else Nothing) decodings
        reqVal <- badReq (decode encodedRequest)
        Right (RequestBodyPresent (Identity reqVal))
      Nothing -> Left (status N.status415)
    RequestBodyAbsent -> case mcontent of
      Just _ -> Left (status N.status415)
      Nothing -> Right RequestBodyAbsent
  return (Concealed route captures querys decodedRequestBody)
  where badReq :: Either T.Text b -> Either TrasaErr b
        badReq = first (TrasaErr N.status400 . LBS.fromStrict . T.encodeUtf8)

-- | Parses only the path.
parsePathWith :: forall route.
     Router route
  -> T.Text -- ^ Method
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
  go captures ps (IxedRouter matches mcapture responders) = case ps of
    [] -> case HM.lookup method responders of
      Nothing -> []
      Just respondersAtMethod ->
        mapMaybe (\(IxedResponder route capDecs) ->
          fmap (\x -> (Pathed route x)) (decodeCaptureVector capDecs captures)
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
parseQueryWith decoding (QueryString querys) = go decoding
  where
    go :: Rec (Query CaptureDecoding) qrys -> Either TrasaErr (Rec Parameter qrys)
    go RNil = Right RNil
    go (q :& qs) = (:&) <$> param <*> go qs
      where
        param = case q of
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

prepareWith ::
     (forall caps qry req resp. route caps qry req resp -> Path pf caps)
  -> (forall caps qry req resp. route caps qry req resp -> Rec (Query qf) qry)
  -> (forall caps qry req resp. route caps qry req resp -> RequestBody rqf req)
  -> route captures query request response
  -> Arguments captures query request (Prepared route response)
prepareWith toPath toQuery toReqBody route =
  prepareExplicit route (toPath route) (toQuery route) (toReqBody route)

prepareExplicit :: forall route captures querys request response rqf pf qf.
     route captures querys request response
  -> Path pf captures
  -> Rec (Query qf) querys
  -> RequestBody rqf request
  -> Arguments captures querys request (Prepared route response)
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
data Constructed :: ([Type] -> [Param] -> Bodiedness -> Type -> Type) -> Type where
  Constructed :: route captures querys request response -> Constructed route
-- I dont really like the name Constructed, but I don't want to call it
-- Some or Any since these get used a lot and a conflict would be likely.
-- Think, think, think.

mapConstructed ::
     (forall caps qrys req resp. sub caps qrys req resp -> route cap qrys req resp)
  -> Constructed sub
  -> Constructed route
mapConstructed f (Constructed sub) = Constructed (f sub)

-- | Only includes the path. Once querystring params get added
--   to this library, this data type should not have them. This
--   type is only used internally and should not be exported.
data Pathed :: ([Type] -> [Param] -> Bodiedness -> Type -> Type) -> Type  where
  Pathed :: route captures querys request response -> Rec Identity captures -> Pathed route

-- | Includes the path and the request body (and the querystring
--   params after they get added to this library).
data Prepared :: ([Type] -> [Param] -> Bodiedness -> Type -> Type) -> Type -> Type where
  Prepared ::
       route captures querys request response
    -> Rec Identity captures
    -> Rec Parameter querys
    -> RequestBody Identity request
    -> Prepared route response

-- | Only needed to implement 'parseWith'. Most users do not need this.
--   If you need to create a route hierarchy to provide breadcrumbs,
--   then you will need this.
data Concealed :: ([Type] -> [Param] -> Bodiedness -> Type -> Type) -> Type where
  Concealed ::
       route captures querys request response
    -> Rec Identity captures
    -> Rec Parameter querys
    -> RequestBody Identity request
    -> Concealed route

-- | Conceal the response type.
conceal :: Prepared route response -> Concealed route
conceal (Prepared route caps querys req) = Concealed route caps querys req

-- | The HTTP content type and body.
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

-- | Only promoted version used.
data Nat = S !Nat | Z

newtype Router route = Router (IxedRouter route 'Z)

data IxedRouter :: ([Type] -> [Param] -> Bodiedness -> Type -> Type) -> Nat -> Type where
  IxedRouter ::
       HashMap T.Text (IxedRouter route n)
    -> Maybe (IxedRouter route ('S n))
    -> HashMap T.Text [IxedResponder route n] -- Should be either zero or one, more than one means that there are trivially overlapped routes
    -> IxedRouter route n

-- | This monoid instance is provided so that we can
--   conveniently use foldMap elsewhere. We do not
--   provide a Monoid instance for Router like we do
--   for IxedRouter. End users only have one way to create
--   a router, and if they combine a Router with itself
--   using mappend, it would result in Router in which all
--   routes were overlapped.
instance Monoid (IxedRouter route n) where
  mempty = IxedRouter HM.empty Nothing HM.empty
  mappend = unionIxedRouter

data IxedResponder :: ([Type] -> [Param] -> Bodiedness -> Type -> Type) -> Nat -> Type where
  IxedResponder ::
       route captures query request response
    -> IxedRec CaptureDecoding n captures
    -> IxedResponder route n

data IxedRec :: (k -> Type) -> Nat -> [k] -> Type where
  IxedRecNil :: IxedRec f 'Z '[]
  IxedRecCons :: !(f r) -> IxedRec f n rs -> IxedRec f ('S n) (r ': rs)

data Vec :: Nat -> Type -> Type where
  VecNil :: Vec 'Z a
  VecCons :: !a -> Vec n a -> Vec ('S n) a

data IxedPath :: (Type -> Type) -> Nat -> [Type] -> Type where
  IxedPathNil :: IxedPath f 'Z '[]
  IxedPathCapture :: f a -> IxedPath f n as -> IxedPath f ('S n) (a ': as)
  IxedPathMatch :: T.Text -> IxedPath f n a -> IxedPath f n a

data LenPath :: Nat -> Type where
  LenPathNil :: LenPath 'Z
  LenPathCapture :: LenPath n -> LenPath ('S n)
  LenPathMatch :: T.Text -> LenPath n -> LenPath n

-- Assumes length is in penultimate position.
data HideIx :: (Nat -> k -> Type) -> k -> Type where
  HideIx :: f n a -> HideIx f a

-- toIxedRec :: Rec f xs -> HideIx (IxedRec f) xs
-- toIxedRec RNil = HideIx IxedRecNil
-- toIxedRec (r :& rs) = case toIxedRec rs of
--   HideIx x -> HideIx (IxedRecCons r x)

snocVec :: a -> Vec n a -> Vec ('S n) a
snocVec a VecNil = VecCons a VecNil
snocVec a (VecCons b vnext) =
  VecCons b (snocVec a vnext)

pathToIxedPath :: Path f xs -> HideIx (IxedPath f) xs
pathToIxedPath PathNil = HideIx IxedPathNil
pathToIxedPath (PathConsCapture c pnext) =
  case pathToIxedPath pnext of
    HideIx ixed -> HideIx (IxedPathCapture c ixed)
pathToIxedPath (PathConsMatch s pnext) =
  case pathToIxedPath pnext of
    HideIx ixed -> HideIx (IxedPathMatch s ixed)

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

snocLenPathMatch :: T.Text -> LenPath n -> LenPath n
snocLenPathMatch s x = case x of
  LenPathNil -> LenPathMatch s LenPathNil
  LenPathMatch t pnext -> LenPathMatch t (snocLenPathMatch s pnext)
  LenPathCapture pnext -> LenPathCapture (snocLenPathMatch s pnext)

snocLenPathCapture :: LenPath n -> LenPath ('S n)
snocLenPathCapture x = case x of
  LenPathNil -> LenPathCapture LenPathNil
  LenPathMatch t pnext -> LenPathMatch t (snocLenPathCapture pnext)
  LenPathCapture pnext -> LenPathCapture (snocLenPathCapture pnext)

reverseLenPathMatch :: LenPath n -> LenPath n
reverseLenPathMatch = go
  where
  go :: forall n. LenPath n -> LenPath n
  go LenPathNil = LenPathNil
  go (LenPathMatch s pnext) = snocLenPathMatch s (go pnext)
  go (LenPathCapture pnext) = snocLenPathCapture (go pnext)

singletonIxedRouter ::
  route captures querys request response -> T.Text -> Path CaptureDecoding captures -> IxedRouter route 'Z
singletonIxedRouter route method capDecs = case pathToIxedPath capDecs of
  HideIx ixedCapDecs ->
    let ixedCapDecsRec = ixedPathToIxedRec ixedCapDecs
        responder = IxedResponder route ixedCapDecsRec
        lenPath = reverseLenPathMatch (ixedPathToLenPath ixedCapDecs)
     in singletonIxedRouterHelper responder method lenPath

singletonIxedRouterHelper ::
  IxedResponder route n -> T.Text -> LenPath n -> IxedRouter route 'Z
singletonIxedRouterHelper responder method path =
  let r = IxedRouter HM.empty Nothing (HM.singleton method [responder])
   in singletonIxedRouterGo r path

singletonIxedRouterGo ::
  IxedRouter route n -> LenPath n -> IxedRouter route 'Z
singletonIxedRouterGo r lp = case lp of
  LenPathNil -> r
  LenPathCapture lpNext -> singletonIxedRouterGo (IxedRouter HM.empty (Just r) HM.empty) lpNext
  LenPathMatch s lpNext -> singletonIxedRouterGo (IxedRouter (HM.singleton s r) Nothing HM.empty) lpNext

unionIxedRouter :: IxedRouter route n -> IxedRouter route n -> IxedRouter route n
unionIxedRouter = go
  where
  go :: forall route n. IxedRouter route n -> IxedRouter route n -> IxedRouter route n
  go (IxedRouter matchesA captureA respsA) (IxedRouter matchesB captureB respsB) =
    IxedRouter
      (HM.unionWith go matchesA matchesB)
      (unionMaybeWith go captureA captureB)
      (HM.unionWith (++) respsA respsB)

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
prettyIxedRouter indent (mnode,IxedRouter matches cap responders) =
  let spaces = L.replicate indent ' '
      nextIndent = if isJust mnode then indent + 2 else indent
      children1 = map (first (Just . ('/' : ) . T.unpack)) (HM.toList matches)
      children2 = maybe [] (\x -> [(Just "/:capture",x)]) cap
   in concat
        [ case mnode of
            Nothing -> if length responders > 0
              then ["/ " ++ showRespondersList responders]
              else []
            Just _ -> []
        , maybe [] (\x -> [x]) $ flip fmap mnode $ \node -> spaces
            ++ node
            ++ (if length responders > 0 then " " ++ showRespondersList responders else "")
        , prettyIxedRouter nextIndent =<< children1
        , prettyIxedRouter nextIndent =<< children2
        ]

showRespondersList :: HashMap T.Text [a] -> String
showRespondersList = id
  . (\x -> "[" ++ x ++ "]")
  . L.intercalate ","
  . map (\(method,xs) -> T.unpack method ++ (if L.length xs > 1 then "*" else ""))
  . HM.toList
