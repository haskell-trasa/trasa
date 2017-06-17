{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module exports symbols that will conflict with the standard prelude.
-- It is recommended to be import qualified or just import 'Method' and use its 'IsString' instance.
module Trasa.Method
  (
  -- * Method
    Method(..)
  , encodeMethod
  , decodeMethod
  -- * Convenience pre defined methods
  , get
  , post
  , head
  , put
  , delete
  , trace
  , connect
  , options
  , patch
  ) where

import Prelude hiding (head)
import Data.Hashable(Hashable(..))
import Data.String (IsString(..))
import Data.CaseInsensitive (CI,foldedCase,mk)
import qualified Data.Text as T

newtype Method = Method
  { unMethod :: CI T.Text
  } deriving (Hashable,Eq,Ord,Show,IsString)

encodeMethod :: Method -> T.Text
encodeMethod = foldedCase . unMethod

decodeMethod :: T.Text -> Method
decodeMethod = Method . mk

get :: Method
get = "GET"

post :: Method
post = "POST"

head :: Method
head = "HEAD"

put :: Method
put = "PUT"

delete :: Method
delete = "DELETE"

trace :: Method
trace = "TRACE"

connect :: Method
connect = "CONNECT"

options :: Method
options = "OPTIONS"

patch :: Method
patch = "PATCH"

