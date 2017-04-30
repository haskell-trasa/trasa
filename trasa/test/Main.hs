{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.Read (readMaybe)
import Trasa.Core
import Data.Vinyl
import Data.Kind (Type)

import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Functor.Identity
import Data.Monoid

import Test.DocTest (doctest)

main :: IO ()
main = do
  putStrLn "\nRUNNING DOCTESTS"
  doctest 
    [ "src/Trasa/Core.hs"
    , "src/Trasa/Tutorial.hs"
    ]
  putStrLn "\nPRETTY ROUTER"
  putStrLn (prettyRouter router)
  putStrLn "\nRUNNING OTHER TESTS"
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

-- todo: add a property test to show that parse and link
-- form a partial isomorphism.
properties :: TestTree
properties = testGroup "Properties"
  [ QC.testProperty "roundtrip link parse" roundtripLinkParse
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "link addition route"
      $ link (prepare AdditionR 12 5) @?= ["add","12","5"]
  , testCase "link left pad route"
      $ link (prepare LeftPadR 5 "foo") @?= ["pad","left","5"]
  , testCase "parse hello route"
      $ parse ["hello"] Nothing @?= Right (conceal (prepare HelloR))
  , testCase "parse addition route"
      $ parse ["add","6","3"] Nothing @?= Right (conceal (prepare AdditionR 6 3))
  ]

data Route :: [Type] -> Bodiedness -> Type -> Type where
  HelloR :: Route '[] Bodyless Int
  AdditionR :: Route '[Int,Int] Bodyless Int
  IdentityR :: Route '[String] Bodyless String
  LeftPadR :: Route '[Int] (Body String) String
  TrickyOneR :: Route '[Int] Bodyless String
  TrickyTwoR :: Route '[Int,Int] Bodyless String
  -- PersonR :: Crud PersonId Person Capture as req resp -> Route as req resp

prepare :: Route cs rq rp -> Arguments cs rq (Prepared Route rp)
prepare = prepareWith (metaPath . meta) (metaRequestBody . meta)

link :: Prepared Route rp -> [T.Text]
link = linkWith (mapPath (CaptureEncoding . captureCodecEncode) . metaPath . meta)

parse :: [T.Text] -> Maybe Content -> Either TrasaErr (Concealed Route)
parse = parseWith
  (mapRequestBody (Many . pure . bodyCodecToBodyDecoding) . metaRequestBody . meta)
  router
  "get"

allRoutes :: [Constructed Route]
allRoutes = 
  [ Constructed HelloR
  , Constructed AdditionR
  , Constructed IdentityR
  , Constructed LeftPadR
  , Constructed TrickyOneR
  , Constructed TrickyTwoR
  ]

router :: Router Route
router = routerWith
  (metaMethod . meta)
  (mapPath (CaptureDecoding . captureCodecDecode) . metaPath . meta)
  allRoutes

data Meta ps rq rp = Meta
  { metaPath :: Path CaptureCodec ps
  , metaRequestBody :: RequestBody BodyCodec rq
  , metaResponseBody :: ResponseBody BodyCodec rp
  , metaMethod :: T.Text
  }

meta :: Route ps rq rp -> Meta ps rq rp
meta x = case x of
  HelloR -> Meta 
    (match "hello" ./ end)
    bodyless (resp bodyInt) "get"
  AdditionR -> Meta 
    (match "add" ./ capture int ./ capture int ./ end)
    bodyless (resp bodyInt) "get"
  IdentityR -> Meta
    (match "identity" ./ capture string ./ end)
    bodyless (resp bodyString) "get"
  LeftPadR -> Meta
    (match "pad" ./ match "left" ./ capture int ./ end)
    (body bodyString) (resp bodyString) "get"
  TrickyOneR -> Meta
    (match "tricky" ./ capture int ./ match "one" ./ end)
    bodyless (resp bodyString) "get"
  TrickyTwoR -> Meta
    (capture int ./ capture int ./ match "two" ./ end)
    bodyless (resp bodyString) "get"

int :: CaptureCodec Int
int = CaptureCodec (T.pack . show) (readMaybe . T.unpack)

string :: CaptureCodec String
string = CaptureCodec T.pack (Just . T.unpack)

bodyString :: BodyCodec String
bodyString = BodyCodec (pure "text/plain") LBSC.pack (Right . LBSC.unpack)

bodyUnit :: BodyCodec ()
bodyUnit = BodyCodec (pure "text/plain") (const "") (const (Right ()))

note :: e -> Maybe a -> Either e a
note e Nothing = Left e
note _ (Just x) = Right x

bodyInt :: BodyCodec Int
bodyInt = BodyCodec (pure "text/plain") (LBSC.pack . show)
                    (note "Could not decode int" . readMaybe . LBSC.unpack)

roundtripLinkParse :: Concealed Route -> Property
roundtripLinkParse c@(Concealed route captures reqBody) =
  (case reqBody of 
    RequestBodyPresent _ -> False
    RequestBodyAbsent -> True
  )
  ==>
  Right c == parse (link (Prepared route captures reqBody)) Nothing

-- This instance is defined only so that the test suite can do
-- its job. It not not neccessary or recommended to write this
-- instance in production code.
instance Eq (Concealed Route) where
  Concealed rt1 ps1 rq1 == Concealed rt2 ps2 rq2 = case (rt1,rt2) of
    (AdditionR,AdditionR) -> ps1 == ps2 && rq1 == rq2
    (IdentityR,IdentityR) -> ps1 == ps2 && rq1 == rq2
    (LeftPadR,LeftPadR) -> case (rq1,rq2) of
      (RequestBodyPresent a, RequestBodyPresent b) -> ps1 == ps2 && a == b
    (TrickyOneR,TrickyOneR) -> ps1 == ps2 && rq1 == rq2
    (TrickyTwoR,TrickyTwoR) -> ps1 == ps2 && rq1 == rq2
    (HelloR,HelloR) -> ps1 == ps2 && rq1 == rq2

instance Arbitrary (Concealed Route) where
  arbitrary = oneof
    [ Concealed AdditionR <$> arbitrary <*> arbitrary
    , Concealed IdentityR <$> arbitrary <*> arbitrary
    , Concealed LeftPadR <$> arbitrary <*> arbitrary
    , Concealed TrickyOneR <$> arbitrary <*> arbitrary
    , Concealed TrickyTwoR <$> arbitrary <*> arbitrary
    , Concealed HelloR <$> arbitrary <*> arbitrary
    ]

instance Show (Concealed Route) where
  show (Concealed r a b) = 
    T.unpack ("/" <> T.intercalate "/" (link (Prepared r a b)))

instance Arbitrary (Rec f '[]) where
  arbitrary = pure RNil

instance (Arbitrary (f r), Arbitrary (Rec f rs)) => Arbitrary (Rec f (r ': rs)) where
  arbitrary = (:&) <$> arbitrary <*> arbitrary

instance Arbitrary (RequestBody f 'Bodyless) where
  arbitrary = pure RequestBodyAbsent

instance Arbitrary a => Arbitrary (RequestBody Identity (Body a)) where
  arbitrary = RequestBodyPresent <$> arbitrary

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

