{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Trasa.Server.Implicit
  (
    serve
  ) where

import Data.Functor.Identity (Identity)
import qualified Network.Wai as WAI

import Trasa.Core
import Trasa.Core.Implicit
import Trasa.Server

serve
  :: forall route requestBodyStrat responseBodyStrat.
     ( HasMeta route
     , HasCaptureDecoding (CaptureStrategy route)
     , HasCaptureDecoding (QueryStrategy route)
     , RequestBodyStrategy route ~ Many requestBodyStrat
     , HasBodyDecoding requestBodyStrat
     , ResponseBodyStrategy route ~ Many responseBodyStrat
     , HasBodyEncoding responseBodyStrat
     , EnumerableRoute route )
  => (forall caps qrys req (resp :: Clarity WAI.Response)
     .  route caps qrys req resp
     -> Rec Identity caps
     -> Rec Parameter qrys
     -> RequestBody Identity req
     -> TrasaT IO (Clarify resp))
  -> WAI.Application
serve makeResponse = serveWith (transformMeta . meta) makeResponse router
  where 
  transformMeta :: forall caps qrys req (resp :: Clarity WAI.Response).
       Meta (CaptureStrategy route) (QueryStrategy route) (RequestBodyStrategy route) (ResponseBodyStrategy route) caps qrys req resp
    -> Meta CaptureDecoding CaptureDecoding (Many BodyDecoding) (Many BodyEncoding) caps qrys req resp
  transformMeta = mapMeta captureDecoding captureDecoding (mapMany bodyDecoding) (mapMany bodyEncoding)


