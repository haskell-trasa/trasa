{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.Read (readMaybe)
import Trasa.Core
import Data.Kind (Type)

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

-- todo: add a property test to show that parse and link
-- form a partial isomorphism.
properties :: TestTree
properties = testGroup "Properties" []

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "link addition route"
      $ link (prepare AdditionR 12 5) @?= ["add","12","5"]
  , testCase "link left pad route"
      $ link (prepare LeftPadR 5 "foo") @?= ["pad","left","5"]
  , testCase "parse addition route"
      $ parse ["add","6","3"] Nothing @?= Just (hideResponseType (prepare AdditionR 6 3))
  ]

data Route :: [Type] -> Bodiedness -> Type -> Type where
  AdditionR :: Route '[Int,Int] Bodyless Int
  IdentityR :: Route '[String] Bodyless String
  LeftPadR :: Route '[Int] (Body String) String
  -- PersonR :: Crud PersonId Person Capture as req resp -> Route as req resp

prepare :: Route cs rq rp -> Arguments cs rq (Prepared Route rp)
prepare = prepareWith (metaPath . meta) (metaRequestBody . meta)

link :: Prepared Route rp -> [String]
link = linkWith (mapPath (CaptureEncoding . captureCodecEncode) . metaPath . meta)

parse :: [String] -> Maybe Content -> Maybe (HiddenPrepared Route)
parse = parseWith 
  (metaMethod . meta)
  (mapPath (CaptureDecoding . captureCodecDecode) . metaPath . meta)
  (mapRequestBody (Many . pure . bodyCodecToBodyDecoding) . metaRequestBody . meta)
  allRoutes
  "get"

allRoutes :: [Constructed Route]
allRoutes = [Constructed AdditionR, Constructed IdentityR, Constructed LeftPadR]

data Meta ps rq rp = Meta
  { metaPath :: Path CaptureCodec ps
  , metaRequestBody :: RequestBody BodyCodec rq
  , metaResponseBody :: ResponseBody BodyCodec rp
  , metaMethod :: String
  }

meta :: Route ps rq rp -> Meta ps rq rp
meta x = case x of
  AdditionR -> Meta 
    (match "add" ./ capture int ./ capture int ./ end)
    bodyless (resp bodyInt) "get"
  IdentityR -> Meta
    (match "identity" ./ capture string ./ end)
    bodyless (resp bodyString) "get"
  LeftPadR -> Meta
    (match "pad" ./ match "left" ./ capture int ./ end)
    (body bodyString) (resp bodyString) "get"

int :: CaptureCodec Int
int = CaptureCodec show readMaybe

string :: CaptureCodec String
string = CaptureCodec id Just

bodyString :: BodyCodec String
bodyString = BodyCodec (pure "text/plain") id Just

bodyInt :: BodyCodec Int
bodyInt = BodyCodec (pure "text/plain") show readMaybe


-- This instance is defined only so that the test suite can do
-- its job. It not not neccessary or recommended to write this
-- instance in production code.
instance Eq (HiddenPrepared Route) where
  HiddenPrepared rt1 ps1 rq1 == HiddenPrepared rt2 ps2 rq2 = case (rt1,rt2) of
    (AdditionR,AdditionR) -> ps1 == ps2 && rq1 == rq2
    (IdentityR,IdentityR) -> ps1 == ps2 && rq1 == rq2
    (LeftPadR,LeftPadR) -> case (rq1,rq2) of
      (RequestBodyPresent a, RequestBodyPresent b) -> ps1 == ps2 && a == b

instance Show (HiddenPrepared Route) where
  show _ = "HiddenPrepared {..}"

instance Eq (Record f '[]) where
  RecordNil == RecordNil = True

instance (Eq (f a), Eq (Record f as)) => Eq (Record f (a ': as)) where
  RecordCons a as == RecordCons b bs = a == b && as == bs

instance Eq (RequestBody f 'Bodyless) where
  RequestBodyAbsent == RequestBodyAbsent = True

-- pieces :: Route cps rq rp -> Path Capture cps
-- pieces x = case x of
--   AdditionR -> match "add" ./ capture int ./ capture int ./ end
--   IdentityR -> match "identity" ./ capture string ./ end
--   LeftPadR -> match "pad" ./ match "left" ./ capture int ./ end
--   PersonR op -> match "person" ./ crudPieces personId op

-- crudPieces :: Capture k -> Crud k v Capture cps rq rp -> Path Capture cps
-- crudPieces cap x = case x of
--   AddR -> match "add" ./ end
--   EditR -> match "edit" ./ capture cap ./ end
--   ViewR -> match "view" ./ capture cap ./ end
-- 
-- newtype PersonId = PersonId { getPersonId :: Int }
-- data Person = Person
-- 
-- personId :: Capture PersonId
-- personId = Capture (show . getPersonId) (fmap PersonId . readMaybe)
-- 
-- data Crud :: Type -> Type -> (Type -> Type) -> [Type] -> Bodiedness -> Type -> Type where
--   AddR :: Crud k v cap '[] (Body Person) PersonId
--   EditR :: Crud k v cap '[k] (Body Person) Person
--   ViewR :: Crud k v cap '[k] Bodyless Person

