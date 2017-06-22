{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Text.Read (readMaybe)
import Trasa.Core
import qualified Trasa.Method as M
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
    [ "src"
    ]
  putStrLn "\nPRETTY ROUTER"
  putStrLn (prettyRouter (router @Route))
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
      $ link (prepare AdditionR 12 5 (Just 3)) @?= decodeUrl "/add/12/5?more=3"
  , testCase "link left pad route"
      $ link (prepare LeftPadR 5 "foo") @?= decodeUrl "/pad/left/5"
  , testCase "parse hello route"
      $ parseUrl "/hello" @?= Right (conceal (prepare HelloR))
  , testCase "parse addition route"
      $ parseUrl "/add/6/3" @?= Right (conceal (prepare AdditionR 6 3 Nothing))
  ]

parseUrl :: T.Text -> Either TrasaErr (Concealed Route)
parseUrl url = parse "GET" (decodeUrl url) Nothing

data Route :: [Type] -> [Param] -> Bodiedness -> Type -> Type where
  EmptyR :: Route '[] '[] Bodyless Int
  HelloR :: Route '[] '[] Bodyless Int
  AdditionR :: Route '[Int,Int] '[Optional Int] Bodyless Int
  IdentityR :: Route '[String] '[] Bodyless String
  LeftPadR :: Route '[Int] '[] (Body String) String
  TrickyOneR :: Route '[Int] '[] Bodyless String
  TrickyTwoR :: Route '[Int,Int] '[] Bodyless String

instance EnumerableRoute Route where
  enumerateRoutes =
    [ Constructed HelloR
    , Constructed AdditionR
    , Constructed IdentityR
    , Constructed LeftPadR
    , Constructed TrickyOneR
    , Constructed TrickyTwoR
    , Constructed EmptyR
    ]

instance HasMeta Route where
  type CaptureStrategy Route = CaptureCodec
  type QueryStrategy Route = CaptureCodec
  type RequestBodyStrategy Route = Many BodyCodec
  type ResponseBodyStrategy Route = Many BodyCodec
  meta :: Route ps qs rq rp -> MetaCodec ps qs rq rp
  meta x = metaBuilderToMetaCodec $ case x of
    EmptyR -> Meta
      end
      qend
      bodyless (resp bodyInt) M.get
    HelloR -> Meta
      (match "hello" ./ end)
      qend
      bodyless (resp bodyInt) M.get
    AdditionR -> Meta
      (match "add" ./ capture int ./ capture int ./ end)
      (optional "more" int .& qend)
      bodyless (resp bodyInt) M.get
    IdentityR -> Meta
      (match "identity" ./ capture string ./ end)
      qend
      bodyless (resp bodyString) M.get
    LeftPadR -> Meta
      (match "pad" ./ match "left" ./ capture int ./ end)
      qend
      (body bodyString) (resp bodyString) M.get
    TrickyOneR -> Meta
      (match "tricky" ./ capture int ./ match "one" ./ end)
      qend
      bodyless (resp bodyString) M.get
    TrickyTwoR -> Meta
      (capture int ./ capture int ./ match "two" ./ end)
      qend
      bodyless (resp bodyString) M.get

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
roundtripLinkParse c@(Concealed route captures querys reqBody) =
  (case reqBody of
    RequestBodyPresent _ -> False
    RequestBodyAbsent -> True
  )
  ==>
  Right c == parseUrl (encodeUrl (link (Prepared route captures querys reqBody)))

-- This instance is defined only so that the test suite can do
-- its job. It not not neccessary or recommended to write this
-- instance in production code.
instance Eq (Concealed Route) where
  Concealed rt1 ps1 qs1 rq1 == Concealed rt2 ps2 qs2 rq2 = case (rt1,rt2) of
    (AdditionR,AdditionR) -> ps1 == ps2 && qs1 == qs2 && rq1 == rq2
    (IdentityR,IdentityR) -> ps1 == ps2 && qs1 == qs2 && rq1 == rq2
    (LeftPadR,LeftPadR) -> case (rq1,rq2) of
      (RequestBodyPresent a, RequestBodyPresent b) -> ps1 == ps2 && qs1 == qs2 && a == b
    (TrickyOneR,TrickyOneR) -> ps1 == ps2 && qs1 == qs2 && rq1 == rq2
    (TrickyTwoR,TrickyTwoR) -> ps1 == ps2 && qs1 == qs2 && rq1 == rq2
    (HelloR,HelloR) -> ps1 == ps2 && qs1 == qs2 && rq1 == rq2
    (EmptyR,EmptyR) -> ps1 == ps2 && qs1 == qs2 && rq1 == rq2

instance Arbitrary (Concealed Route) where
  arbitrary = oneof
    [ Concealed AdditionR <$> arbitrary <*> arbitrary <*> arbitrary
    , Concealed IdentityR <$> arbitrary <*> arbitrary <*> arbitrary
    , Concealed LeftPadR <$> arbitrary <*> arbitrary <*> arbitrary
    , Concealed TrickyOneR <$> arbitrary <*> arbitrary <*> arbitrary
    , Concealed TrickyTwoR <$> arbitrary <*> arbitrary <*> arbitrary
    , Concealed HelloR <$> arbitrary <*> arbitrary <*> arbitrary
    , Concealed EmptyR <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Show (Concealed Route) where
  show (Concealed r a q b) = show (link (Prepared r a q b))

instance Eq a => Eq (Parameter (Optional a)) where
  ParameterOptional m1 == ParameterOptional m2 = m1 == m2

instance Arbitrary (Rec Identity '[]) where
  arbitrary = pure RNil

instance (Arbitrary r, Arbitrary (Rec Identity rs)) => Arbitrary (Rec Identity (r ': rs)) where
  arbitrary = (:&) <$> (Identity <$> arbitrary) <*> arbitrary

instance Arbitrary (Rec Parameter '[]) where
  arbitrary = pure RNil

instance (Arbitrary r, Arbitrary (Rec Parameter rs)) => Arbitrary (Rec Parameter (Optional r ': rs)) where
  arbitrary = (:&) <$> (ParameterOptional <$> arbitrary) <*> arbitrary

instance Arbitrary (RequestBody f 'Bodyless) where
  arbitrary = pure RequestBodyAbsent

instance Arbitrary a => Arbitrary (RequestBody Identity (Body a)) where
  arbitrary = RequestBodyPresent . Identity <$> arbitrary

instance Eq (RequestBody f 'Bodyless) where
  RequestBodyAbsent == RequestBodyAbsent = True
