{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Trasa.Reflex.Implicit
  (
    request
  , requestMany
  , serve
  ) where

import Data.Functor.Identity (Identity)
import Reflex.Dom (MonadWidget,Event)
import Trasa.Core hiding (requestWith)
import Trasa.Core.Implicit
import Trasa.Reflex

request
  :: ( MonadWidget t m
     , HasMeta route
     , HasCaptureEncoding (CaptureStrategy route)
     , HasCaptureEncoding (QueryStrategy route)
     , RequestBodyStrategy route ~ Many requestBodyStrat
     , HasBodyEncoding requestBodyStrat
     , ResponseBodyStrategy route ~ Many responseBodyStrat
     , HasBodyDecoding responseBodyStrat
     )
  => Event t (Prepared route response)
  -> m (Event t (Either TrasaErr response))
request = requestWith (transMeta . meta)
  where transMeta = mapMeta captureEncoding captureEncoding (mapMany bodyEncoding) (mapMany bodyDecoding)

requestMany
  :: ( MonadWidget t m
     , Traversable f
     , HasMeta route
     , HasCaptureEncoding (CaptureStrategy route)
     , HasCaptureEncoding (QueryStrategy route)
     , RequestBodyStrategy route ~ Many requestBodyStrat
     , HasBodyEncoding requestBodyStrat
     , ResponseBodyStrategy route ~ Many responseBodyStrat
     , HasBodyDecoding responseBodyStrat
     )
  => Event t (f (Prepared route response))
  -> m (Event t (f (Either TrasaErr response)))
requestMany = requestManyWith (transMeta . meta)
  where transMeta = mapMeta captureEncoding captureEncoding (mapMany bodyEncoding) (mapMany bodyDecoding)

serve
  :: ( MonadWidget t m
     , HasMeta route
     , HasCaptureEncoding (CaptureStrategy route)
     , HasCaptureDecoding (CaptureStrategy route)
     , HasCaptureCodec (QueryStrategy route)
     , RequestBodyStrategy route ~ Many requestBodyStrat
     , HasBodyCodec requestBodyStrat
     , ResponseBodyStrategy route ~ Many responseBodyStrat
     , HasBodyDecoding responseBodyStrat
     , EnumerableRoute route
     )
  => (forall caps qrys req resp.
      route caps qrys req resp ->
      Rec Identity caps ->
      Rec Parameter qrys ->
      ResponseBody Identity resp ->
      m (Event t (Concealed route)))
  -> (TrasaErr -> m (Event t (Concealed route)))
  -> m ()
serve = serveWith (transMeta . meta) router
  where transMeta = mapMeta captureEncoding captureCodec (mapMany bodyCodec) (mapMany bodyDecoding)
