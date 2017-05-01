{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| Users of this library should a data type representing all possible
    routes available in a web application. It is recommended that
    this type be named @Route@, but this is not required.
-}

module Trasa.Tutorial 
  ( -- * Dispatch and Routing
    -- $dispatchandrouting
  ) where

import Trasa.Core
import Data.Kind (Type)
import Data.Text (Text)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XKindSignatures
-- >>> :set -XGADTs
-- >>> :set -XOverloadedStrings

-- $dispatchandrouting
-- In this example, we will write web application that maintains three
-- counters. The end user will be able to perform various operations
-- that manipulate the values of these counters and ask for their
-- current value. We begin by defining our route type:
--
-- >>> :{
-- data Counter = Red | Green | Blue
--   deriving (Show,Read)
-- data Route :: [Type] -> Bodiedness Type -> Type -> Type where
--   AssignR :: Route '[Counter,Int] 'Bodyless ()
--   IncrementR :: Route '[Counter] 'Bodyless Int
--   QueryR :: Route '[Counter] 'Bodyless Int
--   TotalR :: Route '[] 'Bodyless Int
-- data Meta cs rq rp = Meta
--   { metaPath :: Path CaptureCodec cs
--   , metaRequestBody :: RequestBody BodyCodec rq
--   , metaResponseBody :: ResponseBody BodyCodec rp
--   , metaMethod :: Text
--   }
-- int :: CaptureCodec Int
-- int = showReadCaptureCodec
-- counter :: CaptureCodec Counter
-- counter = showReadCaptureCodec
-- bodyUnit :: BodyCodec ()
-- bodyUnit = BodyCodec (pure "text/plain") (const "") (const (Right ()))
-- bodyInt :: BodyCodec Int
-- bodyInt = showReadBodyCodec
-- meta :: Route ps rq rp -> Meta ps rq rp
-- meta x = case x of
--   AssignR -> Meta 
--     (match "assign" ./ capture counter ./ match "to" ./ capture int ./ end)
--     bodyless (resp bodyUnit) "post"
--   IncrementR -> Meta
--     (match "increment" ./ capture counter ./ end)
--     bodyless (resp bodyInt) "post"
--   QueryR -> Meta
--     (match "query" ./ capture counter ./ end)
--     bodyless (resp bodyInt) "get"
--   TotalR -> Meta
--     (match "total" ./ end)
--     bodyless (resp bodyInt) "get"
-- :}
-- 
-- Now, we can start using our routes. To do this, we take functions that
-- @trasa@ exports and partially apply them to the route metadata that
-- we have created. We can start with prepare and link:
--
-- >>> prepare = prepareWith (metaPath . meta) (metaRequestBody . meta)
-- >>> :t prepare
-- prepare :: Route cs rq rp -> Arguments cs rq (Prepared Route rp)
-- >>> link = linkWith (mapPath (CaptureEncoding . captureCodecEncode) . metaPath . meta)
-- >>> :t link
-- link :: Prepared Route rp -> [Text]
--
-- Now we can use link to encode our routes:
--
-- >>> link (prepare AssignR Green 5)
-- ["assign","Green","to","5"]
--
--

