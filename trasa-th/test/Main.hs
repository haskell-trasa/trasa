{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Trasa.Core
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
      [CaptureRep 'int, CaptureRep 'int, MatchRep "foobar"]
      [QueryRep "int" (OptionalRep 'int)]
      BodylessRep
      'bodyInt
  , RouteRep
      "Blog"
      "POST"
      []
      [QueryRep "id" (ListRep 'int)]
      (BodyRep 'bodyString)
      'bodyUnit
  ]))

[parseTrasa|
Type: ParsedRoute
ParsedAdd GET /add/:int/:int?three=int Bodyless int
|]

main :: IO ()
main = do
  print (link (prepare Add 1 1 (Just 1)))
  print (link (prepare Blog [1,2,3] "This is a post"))
