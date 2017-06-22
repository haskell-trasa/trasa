{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Trasa.Test where

import Data.String -- How do we go from a string to a method
import Trasa.Core
import Trasa.TH

test
