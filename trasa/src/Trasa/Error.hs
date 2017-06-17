{-# LANGUAGE OverloadedStrings #-}
module Trasa.Error
  (
  -- * Types
    TrasaErr(..)
  -- * Simple Errors
  , status
  ) where

import Control.Exception (Exception(..))
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Network.HTTP.Types as N

data TrasaErr = TrasaErr
  { trasaErrStatus :: N.Status
  , trasaErrBody :: LBC.ByteString
  } deriving (Eq,Ord)

instance Show TrasaErr where
  show (TrasaErr s b) =
    "Trasa Error with status: " ++
    show s ++
    if LBC.null b then "" else " and body: " ++ LBC.unpack b

instance Exception TrasaErr where

status :: N.Status -> TrasaErr
status s = TrasaErr s ""

