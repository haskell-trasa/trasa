{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
module Trasa.Client.Implicit where

import Trasa.Core
import Trasa.Core.Implicit
import Trasa.Client
import Data.Kind (Type)

client
  :: forall (k :: Type) (route :: [Type] -> [Param] -> Bodiedness -> Clarity k -> Type) requestBodyStrat responseBodyStrat (response :: Type).
     ( HasMeta route
     , HasCaptureEncoding (CaptureStrategy route)
     , HasCaptureEncoding (QueryStrategy route)
     , RequestBodyStrategy route ~ Many requestBodyStrat
     , HasBodyEncoding requestBodyStrat
     , ResponseBodyStrategy route ~ Many responseBodyStrat
     , HasBodyDecoding responseBodyStrat
     )
  => Config
  -> Prepared route (Clear response :: Clarity k)
  -> IO (Either TrasaErr response)
client = clientWith (transformMeta . meta)
  where 
  transformMeta :: forall caps qrys req (resp :: Type).
       Meta (CaptureStrategy route) (QueryStrategy route) (RequestBodyStrategy route) (ResponseBodyStrategy route) caps qrys req (Clear resp :: Clarity k)
    -> Meta CaptureEncoding CaptureEncoding (Many BodyEncoding) (Many BodyDecoding) caps qrys req (Clear resp :: Clarity k)
  transformMeta = mapMeta captureEncoding captureEncoding (mapMany bodyEncoding) (mapMany bodyDecoding)


