{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Werror -ddump-splices #-}
module Main where

import Data.List.NonEmpty
import Trasa.Core
import Trasa.Core.Implicit
import Trasa.TH

int :: CaptureEncoding Int
int = captureCodecToCaptureEncoding showReadCaptureCodec

bodyInt :: BodyCodec Int
bodyInt = showReadBodyCodec

bodyString :: BodyCodec String
bodyString = showReadBodyCodec

bodyUnit :: BodyCodec ()
bodyUnit = showReadBodyCodec

$(trasa (
  RoutesRep
  "Route"
  [ RouteRep
      "Add"
      "GET"
      [MatchRep "add",CaptureRep 'int, CaptureRep 'int]
      [QueryRep "third" (OptionalRep 'int)]
      []
      ('bodyInt :| [])
  , RouteRep
      "Blog"
      "POST"
      [MatchRep "blog"]
      [QueryRep "id" (ListRep 'int)]
      ['bodyString]
      ('bodyUnit :| [])
  ]))


-- [parseTrasa|
--  data-type: ParsedRoute
--  ParsedAdd GET /add/:int/:int?third=int [] bodyInt
--  ParsedBlog POST /?id=[int] [bodyString] bodyUnit
-- |]

main :: IO ()
main = do
  print (link (prepare Add 1 1 (Just 1)))
  print (link (prepare Blog [1,2,3] "This is a post"))
