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
import qualified Trasa.Method as M
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
-- data Route :: [Type] -> [Param] -> Bodiedness -> Type -> Type where
--   AssignR :: Route '[Counter,Int] '[] 'Bodyless ()
--   IncrementR :: Route '[Counter] '[] 'Bodyless Int
--   QueryR :: Route '[Counter] '[]Bodyless Int
--   TotalR :: Route '[] '[] 'Bodyless Int
-- int :: CaptureCodec Int
-- int = showReadCaptureCodec
-- counter :: CaptureCodec Counter
-- counter = showReadCaptureCodec
-- bodyUnit :: BodyCodec ()
-- bodyUnit = BodyCodec (pure "text/plain") (const "") (const (Right ()))
-- bodyInt :: BodyCodec Int
-- bodyInt = showReadBodyCodec
-- meta :: Route captures querys request response -> MetaCodec captures querys request response
-- meta x = metaBuilderToMetaCodec $ case x of
--   AssignR -> Meta
--     (match "assign" ./ capture counter ./ match "to" ./ capture int ./ end)
--     qend
--     bodyless (resp bodyUnit) M.post
--   IncrementR -> Meta
--     (match "increment" ./ capture counter ./ end)
--     qend
--     bodyless (resp bodyInt) M.post
--   QueryR -> Meta
--     (match "query" ./ capture counter ./ end)
--     qend
--     bodyless (resp bodyInt) M.get
--   TotalR -> Meta
--     (match "total" ./ end)
--     qend
--     bodyless (resp bodyInt) M.get
-- :}
--
-- Now, we can start using our routes. To do this, we take functions that
-- @trasa@ exports and partially apply them to the route metadata that
-- we have created. We can start with prepare and link:
--
-- >>> prepare = prepareWith meta
-- >>> :t prepare
-- prepare
--   :: Route captures query request response
--      -> Arguments captures query request (Prepared Route response)
-- >>> :{
-- link = linkWith (metaCodecToMetaClient . meta)
-- :}
--
-- >>> :t link
-- link :: Prepared Route response -> Url
--
-- Now we can use link to encode our routes:
--
-- >>> link (prepare AssignR Green 5)
-- "/assign/Green/to/5"
--
--
