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
  , Router
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
import Data.Maybe (mapMaybe,listToMaybe)
import Data.List.NonEmpty (NonEmpty)
import Control.Monad
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Foldable (toList)
import Data.Bifunctor (first)
import Text.Read (readEither,readMaybe)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types.Status as N
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Vinyl (Rec(..))
import Data.Vinyl.TypeLevel (type (++))

-- $setup
-- >>> :set -XTypeInType

data Bodiedness t = Body t | Bodyless

data RequestBody :: (Type -> Type) -> Bodiedness Type -> Type where
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

appendPath :: Path f as -> Path f bs -> Path f (as ++ bs)
appendPath PathNil bs = bs
appendPath (PathConsMatch a as) bs = PathConsMatch a (appendPath as bs)
appendPath (PathConsCapture cas as) bs = PathConsCapture cas (appendPath as bs)

newtype Many f a = Many { getMany :: NonEmpty (f a) }
  deriving (Functor)

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

captureCodecToCaptureEncoding :: CaptureCodec a -> CaptureEncoding a
captureCodecToCaptureEncoding (CaptureCodec enc _) = CaptureEncoding enc

captureCodecToCaptureDecoding :: CaptureCodec a -> CaptureDecoding a
captureCodecToCaptureDecoding (CaptureCodec _ dec) = CaptureDecoding dec

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
  { trasaErrStatus :: N.Status
  , trasaErrBody :: LBS.ByteString
  } deriving (Show,Eq,Ord)

status :: N.Status -> TrasaErr
status s = TrasaErr s ""

dispatchWith :: forall rt m.
     Applicative m
  => (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyDecoding) rq')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody (Many BodyEncoding) rp')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Rec Identity cs' -> RequestBody Identity rq' -> m rp')
  -> Router rt -- ^ Router
  -> T.Text -- ^ Method
  -> [T.Text] -- ^ Accept headers
  -> [T.Text] -- ^ Path Pieces
  -> Maybe Content -- ^ Content type and request body
  -> m (Either TrasaErr LBS.ByteString) -- ^ Encoded response
dispatchWith toReqBody toRespBody makeResponse router method accepts encodedPath mcontent = sequenceA $ do
  Concealed route decodedPathPieces decodedRequestBody <- parseWith
    toReqBody router method encodedPath mcontent
  let response = makeResponse route decodedPathPieces decodedRequestBody
      ResponseBody (Many encodings) = toRespBody route
  encode <- mapFindE (status N.status406)
    (\(BodyEncoding names encode) ->
       if any (flip elem accepts) names then Just encode else Nothing)
    encodings
  Right (fmap encode response)

routerWith :: 
     (forall cs' rq' rp'. rt cs' rq' rp' -> T.Text)
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureDecoding cs')
  -> [Constructed rt]
  -> Router rt
routerWith toMethod toCapDec enumeratedRoutes = foldr 
  (\(Constructed route) r -> unionRouter r (Router (HM.singleton (toMethod route) (singletonIxedRouter route (toCapDec route)))))
  (Router HM.empty)
  enumeratedRoutes

-- | Parses the path, the querystring (once this gets added), and
--   the request body.
parseWith :: forall rt.
     (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyDecoding) rq')
  -> Router rt -- ^ Router
  -> T.Text -- ^ Request Method
  -> [T.Text] -- ^ Path Pieces
  -> Maybe Content -- ^ Request content type and body
  -> Either TrasaErr (Concealed rt)
parseWith toReqBody router method encodedPath mcontent = do
  Pathed route captures <- maybe (Left (status N.status404)) Right
    $ parsePathWith router method encodedPath
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
  return (Concealed route captures decodedRequestBody)
  where badReq :: Either T.Text b -> Either TrasaErr b
        badReq = first (TrasaErr N.status400 . LBS.fromStrict . T.encodeUtf8)

-- | Parses only the path.
parsePathWith :: forall rt. 
     Router rt 
  -> T.Text -- ^ Method
  -> [T.Text] -- ^ Path Pieces
  -> Maybe (Pathed rt)
parsePathWith (Router hm0) method pieces0 = do
  r0 <- HM.lookup method hm0
  listToMaybe (go VecNil pieces0 r0)
  where
  go :: forall n.
        Vec n T.Text -- captures being accumulated
     -> [T.Text] -- remaining pieces
     -> IxedRouter rt n -- router fragment
     -> [Pathed rt]
  go captures ps (IxedRouter matches mcapture responders) = case ps of
    [] -> mapMaybe (\(IxedResponder rt capDecs) -> 
        fmap (\x -> (Pathed rt x)) (decodeCaptureVector capDecs captures)
      ) responders
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

decodeCaptureVector :: 
     IxedRec CaptureDecoding n xs 
  -> Vec n T.Text
  -> Maybe (Rec Identity xs)
decodeCaptureVector IxedRecNil VecNil = Just RNil
decodeCaptureVector (IxedRecCons (CaptureDecoding decode) rnext) (VecCons piece vnext) = do
  val <- decode piece
  vals <- decodeCaptureVector rnext vnext
  return (Identity val :& vals)

-- | A closed, total type family provided as a convenience to end users.
--   Other function is this library take advantage of 'Arguments' to allow
--   end users use normal function application. Without this, users would 
--   need to write out 'Record' and 'RequestBody' values by hand, which
--   is tedious.
--
--   >>> :kind! Arguments '[Int,Bool] 'Bodyless Double
--   Arguments '[Int,Bool] 'Bodyless Double :: *
--   = Int -> Bool -> Double
type family Arguments (pieces :: [Type]) (body :: Bodiedness Type) (result :: Type) :: Type where
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

-- | A route with all types hidden: the captures, the request body,
--   and the response body. This is needed so that users can
--   enumerate over all the routes.
data Constructed :: ([Type] -> Bodiedness Type -> Type -> Type) -> Type where
  Constructed :: forall rt cps rq rp. rt cps rq rp -> Constructed rt
-- I dont really like the name Constructed, but I don't want to call it
-- Some or Any since these get used a lot and a conflict would be likely.
-- Think, think, think.

mapConstructed ::
     (forall captures request response.
      sub captures request response -> route captures request response)
  -> Constructed sub
  -> Constructed route
mapConstructed f (Constructed sub) = Constructed (f sub)

-- | Only includes the path. Once querystring params get added
--   to this library, this data type should not have them. This
--   type is only used internally and should not be exported.
data Pathed :: ([Type] -> Bodiedness Type -> Type -> Type) -> Type  where
  Pathed :: forall rt cps rq rp. rt cps rq rp -> Rec Identity cps -> Pathed rt

-- | Includes the path and the request body (and the querystring
--   params after they get added to this library).
data Prepared :: ([Type] -> Bodiedness Type -> Type -> Type) -> Type -> Type where
  Prepared :: forall rt ps rq rp.
       rt ps rq rp
    -> Rec Identity ps
    -> RequestBody Identity rq
    -> Prepared rt rp

-- | Only needed to implement 'parseWith'. Most users do not need this.
--   If you need to create a route hierarchy to provide breadcrumbs,
--   then you will need this.
data Concealed :: ([Type] -> Bodiedness Type -> Type -> Type) -> Type where
  Concealed :: forall rt ps rq rp.
       rt ps rq rp
    -> Rec Identity ps
    -> RequestBody Identity rq
    -> Concealed rt

-- | Conceal the response type.
conceal :: Prepared rt rp -> Concealed rt
conceal (Prepared a b c) = Concealed a b c

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

newtype Router rt = Router (HashMap T.Text (IxedRouter rt 'Z))

data IxedRouter :: ([Type] -> Bodiedness Type -> Type -> Type) -> Nat -> Type where
  IxedRouter :: 
       HashMap T.Text (IxedRouter rt n) 
    -> Maybe (IxedRouter rt ('S n))
    -> [IxedResponder rt n] -- Should be either zero or one, more than one means that there are trivially overlapped routes
    -> IxedRouter rt n

-- | This monoid instance is provided so that we can
--   conveniently use foldMap elsewhere. We do not
--   provide a Monoid instance for Router like we do 
--   for IxedRouter. End users only have one way to create
--   a router, and if they combine a Router with itself
--   using mappend, it would result in Router in which all
--   routes were overlapped.
instance Monoid (IxedRouter rt n) where
  mempty = IxedRouter HM.empty Nothing []
  mappend = unionIxedRouter
  
data IxedResponder :: ([Type] -> Bodiedness Type -> Type -> Type) -> Nat -> Type where
  IxedResponder :: forall rt cs rq rp n.
       rt cs rq rp
    -> IxedRec CaptureDecoding n cs
    -> IxedResponder rt n

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
     rt cs rq rp -> Path CaptureDecoding cs -> IxedRouter rt 'Z
singletonIxedRouter route capDecs = case pathToIxedPath capDecs of
  HideIx ixedCapDecs ->
    let ixedCapDecsRec = ixedPathToIxedRec ixedCapDecs
        responder = IxedResponder route ixedCapDecsRec 
        lenPath = reverseLenPathMatch (ixedPathToLenPath ixedCapDecs)
     in singletonIxedRouterHelper responder lenPath

singletonIxedRouterHelper :: 
  IxedResponder rt n -> LenPath n -> IxedRouter rt 'Z
singletonIxedRouterHelper responder path = 
  let r = IxedRouter HM.empty Nothing [responder]
   in singletonIxedRouterGo r path

singletonIxedRouterGo ::
  IxedRouter rt n -> LenPath n -> IxedRouter rt 'Z
singletonIxedRouterGo r lp = case lp of
  LenPathNil -> r
  LenPathCapture lpNext -> singletonIxedRouterGo (IxedRouter HM.empty (Just r) []) lpNext
  LenPathMatch s lpNext -> singletonIxedRouterGo (IxedRouter (HM.singleton s r) Nothing []) lpNext

unionIxedRouter :: IxedRouter rt n -> IxedRouter rt n -> IxedRouter rt n
unionIxedRouter = go
  where
  go :: forall rt n. IxedRouter rt n -> IxedRouter rt n -> IxedRouter rt n
  go (IxedRouter matchesA captureA respsA) (IxedRouter matchesB captureB respsB) =
    IxedRouter
      (HM.unionWith go matchesA matchesB)
      (unionMaybeWith go captureA captureB)
      (respsA ++ respsB)

unionRouter :: Router rt -> Router rt -> Router rt
unionRouter (Router a) (Router b) = 
  Router (HM.unionWith unionIxedRouter a b)

unionMaybeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybeWith f x y = case x of
  Nothing -> y
  Just xval -> case y of
    Nothing -> x
    Just yval -> Just (f xval yval)

prettyRouter :: Router rt -> String
prettyRouter (Router hm) = unlines $ join $ map 
  (\(method,r) -> prettyIxedRouter 0 (T.unpack (T.toUpper method), r))
  (HM.toList hm)

prettyIxedRouter :: 
     Int -- ^ Indentation
  -> (String, IxedRouter rt n)
  -> [String]
prettyIxedRouter indent (node,IxedRouter matches cap completions) =
  let spaces = L.replicate indent ' '
      children1 = map (first (('/' : ) . T.unpack)) (HM.toList matches)
      children2 = maybe [] (\x -> [("/:capture",x)]) cap
   in concat
        [ (\x -> [x]) $ spaces 
           ++ node
           ++ (case compare (length completions) 1 of
                 EQ -> " *"
                 GT -> " **"
                 LT -> ""
              )
        , prettyIxedRouter (indent + 2) =<< children1
        , prettyIxedRouter (indent + 2) =<< children2
        ]

