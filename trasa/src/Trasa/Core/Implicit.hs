{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
module Trasa.Core.Implicit
  (
    HasMeta(..)
  , prepare
  , link
  , parse
  , EnumerableRoute(..)
  , router
  ) where

import Data.Kind (Type)

import Trasa.Core

class HasMeta route where
  type CaptureStrategy route :: Type -> Type
  type QueryStrategy route :: Type -> Type
  type RequestBodyStrategy route :: Type -> Type
  type ResponseBodyStrategy route :: Type -> Type
  meta
    :: route caps qrys req resp
    -> Meta (CaptureStrategy route) (QueryStrategy route) (RequestBodyStrategy route) (ResponseBodyStrategy route) caps qrys req resp

prepare
  :: HasMeta route
  => route captures queries request response
  -> Arguments captures queries request (Prepared route response)
prepare = prepareWith meta

link
  :: (HasMeta route, HasCaptureEncoding (CaptureStrategy route), HasCaptureEncoding (QueryStrategy route))
  => Prepared route response
  -> Url
link = linkWith toMeta
  where
    toMeta route = m
      { metaPath = mapPath captureEncoding (metaPath m)
      , metaQuery = mapQuery captureEncoding (metaQuery m)
      }
      where m = meta route

parse
  :: ( HasMeta route
     , HasCaptureDecoding (CaptureStrategy route)
     , HasCaptureDecoding (QueryStrategy route)
     , RequestBodyStrategy route ~ Many strat
     , HasBodyDecoding strat
     , EnumerableRoute route )
  => Method -- ^ Request Method
  -> Url -- ^ Everything after the authority
  -> Maybe Content -- ^ Request content type and body
  -> Either TrasaErr (Concealed route)
parse = parseWith (mapMetaQuery captureDecoding . mapMetaRequestBody (mapMany bodyDecoding) . meta) router

class EnumerableRoute route where
  enumerateRoutes :: [Constructed route]

router
  :: (HasMeta route, HasCaptureDecoding (CaptureStrategy route), EnumerableRoute route)
  => Router route
router = routerWith (mapMetaPath captureDecoding . meta) enumerateRoutes
