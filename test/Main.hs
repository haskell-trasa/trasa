{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

import Text.Read (readMaybe)
import Trasa.Core
import Data.Kind (Type)

main :: IO ()
main = putStrLn "done"

data Route :: [Type] -> Bodiedness -> Type -> Type where
  AdditionR :: Route '[Int,Int] Bodyless Int
  IdentityR :: Route '[String] Bodyless String
  LeftPadR :: Route '[Int] (Body String) String
  -- PersonR :: Crud PersonId Person Capture as req resp -> Route as req resp

-- link :: Route cs rq rp -> HList cs -> [String]

data Meta ps rq rp = Meta
  { metaPath :: Path CaptureCodec ps
  , metaRequestBody :: RequestBody BodyCodec rq
  , metaResponseBody :: ResponseBody BodyCodec rp
  }

meta :: Route ps rq rp -> Meta ps rq rp
meta x = case x of
  AdditionR -> Meta 
    (match "add" ./ capture int ./ capture int ./ end)
    bodyless (response bodyInt)
  IdentityR -> Meta
    (match "identity" ./ capture string ./ end)
    bodyless (response bodyString)
  LeftPadR -> Meta
    (match "pad" ./ match "left" ./ capture int ./ end)
    (body bodyString) (response bodyString)

int :: CaptureCodec Int
int = CaptureCodec show readMaybe

string :: CaptureCodec String
string = CaptureCodec id Just

bodyString :: BodyCodec String
bodyString = BodyCodec ["text/plain"] id Just

bodyInt :: BodyCodec Int
bodyInt = BodyCodec ["text/plain"] show readMaybe

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

