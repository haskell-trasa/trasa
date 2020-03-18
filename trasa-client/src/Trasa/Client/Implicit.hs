{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Trasa.Client.Implicit where

import Trasa.Core
import Trasa.Core.Implicit
import Trasa.Client

client
  :: ( HasMeta route
     , HasCaptureEncoding (CaptureStrategy route)
     , HasCaptureEncoding (QueryStrategy route)
     , RequestBodyStrategy route ~ Many requestBodyStrat
     , HasBodyEncoding requestBodyStrat
     , ResponseBodyStrategy route ~ Many responseBodyStrat
     , HasBodyDecoding responseBodyStrat
     )
  => Config
  -> Prepared route response
  -> IO (Either TrasaErr response)
client = clientWith (transformMeta . meta)
  where transformMeta = mapMeta captureEncoding captureEncoding (mapMany bodyEncoding) (mapMany bodyDecoding)


