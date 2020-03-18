{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

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
  :: ( HasMeta route
     , HasCaptureDecoding (CaptureStrategy route)
     , HasCaptureDecoding (QueryStrategy route)
     , RequestBodyStrategy route ~ Many requestBodyStrat
     , HasBodyDecoding requestBodyStrat
     , ResponseBodyStrategy route ~ Many responseBodyStrat
     , HasBodyEncoding responseBodyStrat
     , EnumerableRoute route )
  => (forall caps qrys req resp
     .  route caps qrys req resp
     -> Rec Identity caps
     -> Rec Parameter qrys
     -> RequestBody Identity req
     -> TrasaT IO resp)
  -> WAI.Application
serve makeResponse = serveWith (transformMeta . meta) makeResponse router
  where transformMeta = mapMeta captureDecoding captureDecoding (mapMany bodyDecoding) (mapMany bodyEncoding)


