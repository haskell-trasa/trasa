{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Trasa.Core 
  ( Bodiedness(..)
  , Path(..)
  , ResponseBody(..)
  , RequestBody(..)
  , BodyCodec(..)
  , BodyDecoding(..)
  , BodyEncoding(..)
  , Prepared(..)
  , Many(..)
  , CaptureCodec(..)
  , CaptureEncoding(..)
  , CaptureDecoding(..)
  -- * Using Routes
  , prepare
  , dispatch
  , parse
  , linkWith
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
  , response
  ) where

import Data.Kind (Type)
import Data.Functor.Identity
import Data.Maybe
-- import Data.List.NonEmpty (NonEmpty)
-- import Data.List

data Bodiedness = Body Type | Bodyless

data RequestBody :: (Type -> Type) -> Bodiedness -> Type where
  RequestBodyPresent :: f a -> RequestBody f ('Body a)
  RequestBodyAbsent :: RequestBody f 'Bodyless

newtype ResponseBody rpf rp = ResponseBody { getResponseBody :: rpf rp }

data Path :: (Type -> Type) -> [Type] -> Type where
  PathNil :: Path cap '[] 
  PathConsCapture :: cap a -> Path cap as -> Path cap (a ': as) 
  PathConsMatch :: String -> Path cap as -> Path cap as 

-- data Endpoint ps req resp
--   { endpointPieces :: Pieces ps req
--   , endpointAccepts :: [Decoding req]
--   , endpointContentTypes :: [Encoding resp]
--   }

-- data Endpoint ps cap req resp = Endpoint
--   { endpointPieces :: Pieces cap ps req
--   , endpointAccepts :: [Decoding req]
--   , endpointContentTypes :: [Encoding resp]
--   }

newtype Many f a = Many { getMany :: [f a] }

data BodyDecoding a = BodyDecoding
  { bodyDecodingNames :: [String]
  , bodyDecodingFunction :: String -> Maybe a
  }

data BodyEncoding a = BodyEncoding
  { bodyEncodingNames :: [String]
  , bodyEncodingFunction :: a -> String
  }

data BodyCodec a = BodyCodec
  { bodyCodecNames :: [String]
  , bodyCodecEncode :: a -> String
  , bodyCodecDecode :: String -> Maybe a
  }


data HList (as :: [Type]) where
  HListNil :: HList '[]
  HListCons :: a -> HList as -> HList (a ': as)

infixr 7 ./

(./) :: (a -> b) -> a -> b
(./) f a = f a

match :: String -> Path cpf ps -> Path cpf ps 
match = PathConsMatch

capture :: cpf cp -> Path cpf cps -> Path cpf (cp ': cps)
capture = PathConsCapture

end :: Path cpf '[]
end = PathNil

body :: rqf rq -> RequestBody rqf ('Body rq)
body = RequestBodyPresent

bodyless :: RequestBody rqf 'Bodyless
bodyless = RequestBodyAbsent

response :: rpf rp -> ResponseBody rpf rp
response = ResponseBody

-- data Fragment (x :: [Type] -> [Type]) where
--   FragmentCapture :: Capture a -> Fragment ('(:) a)
  

data CaptureCodec a = CaptureCodec
  { captureCodecEncode :: a -> String
  , captureCodecDecode :: String -> Maybe a
  }

newtype CaptureEncoding a = CaptureEncoding { appCaptureEncoding :: a -> String }
newtype CaptureDecoding a = CaptureDecoding { appCaptureDecoding :: String -> Maybe a }

-- | Uses an HList.
linkWith :: forall rt rq cs rp.
  (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureEncoding cs') 
  -> rt cs rq rp -> HList cs -> [String]
linkWith toPieces route = encodePieces (toPieces route)

encodePieces :: Path CaptureEncoding cps -> HList cps -> [String]
encodePieces = go
  where
  go :: forall cps. Path CaptureEncoding cps -> HList cps -> [String]
  go PathNil HListNil = []
  go (PathConsMatch str ps) xs = str : go ps xs
  go (PathConsCapture (CaptureEncoding enc) ps) (HListCons x xs) = enc x : go ps xs

dispatch :: forall rt m.
     Functor m
  => (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureDecoding cs') 
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyDecoding) rq') 
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody (Many BodyEncoding) rp') 
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> HList cs' -> RequestBody Identity rq' -> m rp')
  -> [String] -- ^ Accept headers
  -> [SomeRoute rt] -- ^ All available routes
  -> m String -- ^ Response in case no route matched
  -> [String] -- ^ Path Pieces
  -> Maybe (String,String) -- ^ Content type and request body
  -> m String -- ^ Encoded response
dispatch toCapDec toReqBody toRespBody makeResponse accepts enumeratedRoutes defEncodedResp encodedPath mreq = 
  fromMaybe defEncodedResp $ do
    Pathed route decodedPathPieces <- parse toCapDec enumeratedRoutes encodedPath
    decodedRequestBody <- case toReqBody route of
      RequestBodyPresent (Many decodings) -> case mreq of
        Just (contentType,encodedRequest) -> do
          decode <- mapFind (\(BodyDecoding names decode) -> if elem contentType names then Just decode else Nothing) decodings
          reqVal <- decode encodedRequest
          Just (RequestBodyPresent (Identity reqVal))
        Nothing -> Nothing
      RequestBodyAbsent -> case mreq of
        Just _ -> Nothing
        Nothing -> Just RequestBodyAbsent
    let resp = makeResponse route decodedPathPieces decodedRequestBody
        ResponseBody (Many encodings) = toRespBody route
    encode <- mapFind (\(BodyEncoding names encode) -> if any (flip elem accepts) names then Just encode else Nothing) encodings
    Just (fmap encode resp)

parse :: forall rt.
     (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureDecoding cs') 
  -> [SomeRoute rt] -- ^ All available routes
  -> [String] -- ^ Path Pieces
  -> Maybe (Pathed rt)
parse toCapDec enumeratedRoutes encodedPath = mapFind
  (\(SomeRoute route) -> fmap (Pathed route) (parseOne (toCapDec route) encodedPath)) 
  enumeratedRoutes

parseOne :: 
     Path CaptureDecoding cps
  -> [String] -- ^ Path Pieces
  -> Maybe (HList cps)
parseOne = go
  where
  go :: forall cps. Path CaptureDecoding cps -> [String] -> Maybe (HList cps)
  go PathNil xs = case xs of
    [] -> Just HListNil
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
      Just (HListCons v vs)

-- dispatchOne :: forall rt rq cs rp m.
--      Applicative m
--   => (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody BodyEncodings rp') 
--   -> (forall cs' rq' rp'. rt cs' rq' rp' -> Pieces Identity cs' -> RequestBody Identity rq' -> m rp')
--   -> [String] -- ^ Accept headers
--   -> rt cs rq rp -- ^ The route to dispatch
--   -> Pieces Identity cs
--   -> RequestBody Identity rq
--   -> m String -- ^ Encoded response
-- dispatchOne toResponseBodyEncodings makeResponse accepts route pieces reqBody = 
--   let rp = makeResponse route pieces reqBody
--       ResponseBody (BodyEncodings rpEncs) = toResponseBodyEncodings route
--       menc = mapFind (\accept -> mapFind
--           (\(BodyEncoding names enc) -> 
--             if elem accept names then Just enc else Nothing
--           ) rpEncs
--         ) accepts
--    in case menc of
--         Nothing -> pure "No valid content type is provided"
--         Just enc -> fmap enc rp


type family Arguments (pieces :: [Type]) (body :: Bodiedness) (result :: Type) :: Type where
  Arguments '[] ('Body b) r = b -> r
  Arguments '[] 'Bodyless r = r
  Arguments (c ': cs) b r = c -> Arguments cs b r

prepare :: 
     (forall cs' rq' rp'. rt cs' rq' rp' -> Path pf cs') 
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody rqf rq') 
  -> rt cs rq rp 
  -> Arguments cs rq (Prepared rt)
prepare toPath toReqBody route = 
  prepareExplicit route (toPath route) (toReqBody route)

prepareExplicit :: forall rt cs rq rp rqf pf.
     rt cs rq rp 
  -> Path pf cs
  -> RequestBody rqf rq
  -> Arguments cs rq (Prepared rt)
prepareExplicit route path req = 
  let (captures, reqBod) = go path req
   in Prepared route captures reqBod
  where
  go :: forall cs'. Path pf cs' -> RequestBody rqf rq 
     -> Arguments cs' rq (HList cs', RequestBody Identity rq)
  go (PathConsCapture _ pnext) b = \x -> 
    let (captures, reqBod) = go pnext b
     in (HListCons x captures, reqBod)
  go (PathConsMatch _ pnext) b = go pnext b
  go PathNil RequestBodyAbsent = (HListNil,RequestBodyAbsent)
  go PathNil (RequestBodyPresent _) = \x -> (HListNil,RequestBodyPresent (Identity x))

data SomeRoute rt where
  SomeRoute :: forall rt cps rq rp. rt cps rq rp -> SomeRoute rt

-- | Only includes the path. Once querystring params get added
--   to this library, this data type should not have them.
data Pathed rt where
  Pathed :: forall rt cps rq rp. rt cps rq rp -> HList cps -> Pathed rt

-- | Includes the path and the request body (and the querystring
--   params after they get added to this library).
data Prepared rt where
  Prepared :: forall rt ps rq rp. 
       rt ps rq rp 
    -> HList ps 
    -> RequestBody Identity rq 
    -> Prepared rt

mapFind :: (a -> Maybe b) -> [a] -> Maybe b
mapFind f = listToMaybe . mapMaybe f

