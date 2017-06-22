{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module Trasa.TH where

import Data.Kind (Type)
import Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH as TH
import Data.Maybe (listToMaybe)
import Trasa.Core

data CodecRep = CodecRep
  { codecRepName :: Name
  , codecRepCodec :: TH.Type
  , codecRepType :: TH.Type
  }

data ParamRep
  = ParamRepFlag
  | ParamRepOptional CodecRep
  | ParamRepList CodecRep

data QueryRep = QueryRep
  { queryRepKey :: String
  , queryRepParam :: ParamRep
  }

data BodiednessRep
  = BodyRep CodecRep
  | BodylessRep

data RouteRep = RouteRep
  { routeRepName :: String
  , routeRepMethod :: String
  , routeRepCaptures :: [CodecRep]
  , routeRepQueries :: [ParamRep]
  , routeReqRequest :: BodiednessRep
  , routeReqResponse :: CodecRep
  }

genCodec :: Name -> Q CodecRep
genCodec name = reify name >>= \case
  VarI fullName fullType _ -> case fullType of
    AppT codec typ -> return (CodecRep fullName codec typ)
    _ -> fail ("Codec: " ++ show name ++ " does not have a type like (Codec Type) but has the type: " ++ show fullType)
  _ -> fail ("Codec: " ++ show name ++ " is not a haskell value")

routeKind :: Q TH.Type
routeKind = [t| [Type] -> [Param] -> Bodiedness -> Type -> Type |]

typeList :: [TH.Type] -> TH.Type
typeList = foldr (\typ rest -> PromotedConsT `AppT` typ `AppT` rest) PromotedNilT

routeType :: [RouteRep] -> Q Dec
routeType routeReps = do
  let route = mkName "Route"
  kind <- routeKind
  return (DataD [] route [] (Just kind) (fmap (buildCons route) routeReps) [])
  where
    buildCons :: Name -> RouteRep -> Con
    buildCons route (RouteRep name _ captures queries request response) = GadtC [mkName name] [] typ
      where
       typ =
        ConT route `AppT`
        typeList (fmap codecRepType captures) `AppT`
        typeList (fmap queryRepToParam queries) `AppT`
        bodiednessRepToBodiedness request `AppT`
        codecRepType response
    queryRepToParam :: ParamRep -> TH.Type
    queryRepToParam = \case
      ParamRepFlag -> PromotedT (mkName "Flag")
      ParamRepOptional (CodecRep _ _ typ) -> PromotedT (mkName "Optional") `AppT` typ
      ParamRepList (CodecRep _ _ typ) -> PromotedT (mkName "List") `AppT` typ
    bodiednessRepToBodiedness :: BodiednessRep -> TH.Type
    bodiednessRepToBodiedness = \case
      BodyRep (CodecRep _ _ typ) -> PromotedT (mkName "Body") `AppT` typ
      BodylessRep -> PromotedT (mkName "Bodyless")

constructeds :: [RouteRep] -> Q [Dec]
constructeds routeReps = do
  constructedType <- [t| Constructed |]
  constructedData <- [| Constructed |]
  let typ = ListT `AppT` (constructedType `AppT` ConT (mkName "Route"))
      allRoutes = mkName "allRoutes"
      buildCons name = constructedData `AppE` ConE (mkName name)
      expr = fmap (buildCons . routeRepName) routeReps
  return [SigD allRoutes typ,FunD allRoutes [Clause [] (NormalB (ListE expr)) []]]


metaType :: [RouteRep] -> Q Dec
metaType routeReps = do
  hlist <- [t| [Type] |]
  param <- [t| [Param] |]
  bodiedness <- [t| Bodiedness |]
  let pathCodec = search routeRepCaptures (Just . codecRepCodec) [t| CaptureCodec |]
  path <- [t| Path $(pathCodec) $(varT (mkName "caps")) |]
  let queryCodec = search routeRepQueries paramCodec [t| CaptureCodec |]
  qrys <- [t| Rec (Query $(queryCodec)) $(varT (mkName "qrys")) |]
  let typeVars =
        [ KindedTV (mkName "caps") hlist
        , KindedTV (mkName "qrys") param
        , KindedTV (mkName "req") bodiedness
        , KindedTV (mkName "resp") StarT
        ]
      strict = Bang NoSourceUnpackedness SourceStrict
      fields =
        [ (mkName "metaPath",strict,path)
        , (mkName "metaQuery",strict,qrys)]
      con = RecC (mkName "Meta") fields
  return (DataD [] (mkName "Meta") typeVars Nothing [con] [])
  where
    search :: (RouteRep -> [b]) -> (b -> Maybe TH.Type) -> Q TH.Type -> Q TH.Type
    search f g err = case listToMaybe routeReps >>= \b -> listToMaybe (f b) >>= g of
      Just t -> return t
      Nothing -> err
    paramCodec :: ParamRep -> Maybe TH.Type
    paramCodec = \case
      ParamRepFlag -> Nothing
      ParamRepOptional (CodecRep _ codec _) -> Just codec
      ParamRepList (CodecRep _ codec _) -> Just codec

trasa :: [RouteRep] -> Q [Dec]
trasa routeReps = do
  route <- routeType routeReps
  cons <- constructeds routeReps
  meta <- metaType routeReps
  return ([route] ++ cons ++ [meta])

int :: CaptureEncoding Int
int = captureCodecToCaptureEncoding showReadCaptureCodec

bodyInt :: BodyCodec Int
bodyInt = showReadBodyCodec

bodyString :: BodyCodec String
bodyString = showReadBodyCodec

bodyUnit :: BodyCodec ()
bodyUnit = showReadBodyCodec

test :: Q [Dec]
test = do
  intC <- genCodec 'int
  bodyIntC <- genCodec 'bodyInt
  bodyStringC <- genCodec 'bodyString
  bodyUnitC <- genCodec 'bodyUnit
  let get = RouteRep "Add" "GET" [intC,intC] [ParamRepOptional intC] BodylessRep bodyIntC
      post = RouteRep "Blog" "POST" [] [ParamRepOptional intC] (BodyRep bodyStringC) bodyUnitC
  trasa [get,post]

example :: IO ()
example = runQ test >>= print . ppr
