{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module Trasa.TH where

import Data.Kind (Type)
import Data.Maybe (listToMaybe,mapMaybe)
import Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH as TH
import Trasa.Core

data CodecRep = CodecRep
  { codecRepName :: Name
  , codecRepCodec :: TH.Type
  , codecRepType :: TH.Type
  }

data CaptureRep codecRep
  = MatchRep String
  | CaptureRep codecRep
  deriving (Foldable,Functor,Traversable)

data ParamRep codecRep
  = FlagRep
  | OptionalRep codecRep
  | ListRep codecRep
  deriving (Foldable,Functor,Traversable)

data QueryRep codecRep = QueryRep
  { queryRepKey :: String
  , queryRepParam :: ParamRep codecRep
  } deriving (Foldable,Functor,Traversable)

data BodiednessRep codecRep
  = BodyRep codecRep
  | BodylessRep
  deriving (Foldable,Functor,Traversable)

data RouteRep codecRep = RouteRep
  { routeRepName :: String
  , routeRepMethod :: String
  , routeRepCaptures :: [CaptureRep codecRep]
  , routeRepQueries :: [QueryRep codecRep]
  , routeReqRequest :: BodiednessRep codecRep
  , routeReqResponse :: codecRep
  } deriving (Foldable,Functor,Traversable)

genCodec :: Name -> Q CodecRep
genCodec name = reify name >>= \case
  VarI fullName fullType _ -> case fullType of
    AppT codec typ -> return (CodecRep fullName codec typ)
    _ -> fail ("Codec: " ++ show name ++ " does not have a type like (Codec Type) but has the type: " ++ show fullType)
  _ -> fail ("Codec: " ++ show name ++ " is not a haskell value")

typeList :: [TH.Type] -> TH.Type
typeList = foldr (\typ rest -> PromotedConsT `AppT` typ `AppT` rest) PromotedNilT

routeType :: [RouteRep CodecRep] -> Q Dec
routeType routeReps = do
  let route = mkName "Route"
  kind <- [t| [Type] -> [Param] -> Bodiedness -> Type -> Type |]
  return (DataD [] route [] (Just kind) (fmap (buildCons route) routeReps) [])
  where
    buildCons :: Name -> RouteRep CodecRep -> Con
    buildCons route (RouteRep name _ captures queries request response) = GadtC [mkName name] [] typ
      where
       typ =
        ConT route `AppT`
        typeList (mapMaybe captureType captures) `AppT`
        typeList (fmap (paramType . queryRepParam) queries) `AppT`
        bodiednessRepToBodiedness request `AppT`
        codecRepType response
    captureType :: CaptureRep CodecRep -> Maybe TH.Type
    captureType = \case
      MatchRep _ -> Nothing
      CaptureRep (CodecRep _ _ typ) -> Just typ
    paramType :: ParamRep CodecRep -> TH.Type
    paramType = \case
      FlagRep -> PromotedT (mkName "Flag")
      OptionalRep (CodecRep _ _ typ) -> PromotedT (mkName "Optional") `AppT` typ
      ListRep (CodecRep _ _ typ) -> PromotedT (mkName "List") `AppT` typ
    bodiednessRepToBodiedness :: BodiednessRep CodecRep -> TH.Type
    bodiednessRepToBodiedness = \case
      BodyRep (CodecRep _ _ typ) -> PromotedT (mkName "Body") `AppT` typ
      BodylessRep -> PromotedT (mkName "Bodyless")

enumRoutesInstance :: [RouteRep CodecRep] -> Q Dec
enumRoutesInstance routeReps = do
  constructedData <- [| Constructed |]
  let typ = ConT (mkName "EnumerableRoute") `AppT` ConT (mkName "Route")
      enumRoutes = mkName "enumerateRoutes"
      buildCons name = constructedData `AppE` ConE (mkName name)
      expr = fmap (buildCons . routeRepName) routeReps
  return (InstanceD Nothing [] typ [FunD enumRoutes [Clause [] (NormalB (ListE expr)) []]])

metaInstance :: [RouteRep CodecRep] -> Q Dec
metaInstance routeReps = do
  let route = mkName "Route"
      typ = ConT (mkName "HasMeta") `AppT` ConT route
  capStrat <- search routeRepCaptures captureCodec [t| CaptureCodec |]
  qryStrat <- search routeRepQueries (paramCodec . queryRepParam) [t| CaptureCodec |]
  reqBodyStrat <- search ((:[]) .routeReqRequest) bodyCodec [t| BodyCodec |]
  respBodyStrat <- search ((:[]) . routeReqResponse) (Just . codecRepCodec) [t| BodyCodec |]
  let mkTypeFamily str strat = TySynInstD (mkName str) (TySynEqn [ConT route] strat)
      typeFamilies =
        [ mkTypeFamily "CaptureStrategy" capStrat
        , mkTypeFamily "QueryStrategy" qryStrat
        , mkTypeFamily "RequestBodyStrategy" reqBodyStrat
        , mkTypeFamily "ResponseBodyStrategy" respBodyStrat
        ]
  lam <- newName "route"
  let metaExp = LamE [VarP lam] (CaseE (VarE lam) (fmap routeRepToMetaPattern routeReps))
      metaFunction = FunD (mkName "meta") [Clause [] (NormalB metaExp) []]
  return (InstanceD Nothing [] typ (typeFamilies ++ [metaFunction]))
  where
    search :: (RouteRep CodecRep -> [b]) -> (b -> Maybe TH.Type) -> Q TH.Type -> Q TH.Type
    search f g err = case listToMaybe routeReps >>= \b -> listToMaybe (f b) >>= g of
      Just t -> return t
      Nothing -> err
    captureCodec :: CaptureRep CodecRep -> Maybe TH.Type
    captureCodec = \case
      MatchRep _ -> Nothing
      CaptureRep (CodecRep _ codec _) -> Just codec
    paramCodec :: ParamRep CodecRep -> Maybe TH.Type
    paramCodec = \case
      FlagRep -> Nothing
      OptionalRep (CodecRep _ codec _) -> Just codec
      ListRep (CodecRep _ codec _) -> Just codec
    bodyCodec :: BodiednessRep CodecRep -> Maybe TH.Type
    bodyCodec = \case
      BodyRep (CodecRep _ codec _) -> Just codec
      BodylessRep -> Nothing
    routeRepToMetaPattern :: RouteRep CodecRep -> Match
    routeRepToMetaPattern (RouteRep name method caps qrys req res) =
      Match (ConP (mkName name) []) (NormalB expr) []
      where
        expr =
          ConE (mkName "Meta") `AppE`
          ParensE capsE `AppE`
          ParensE qrysE `AppE`
          ParensE reqE `AppE`
          ParensE respE `AppE`
          ParensE methodE
        capsE = foldr (\cp -> UInfixE (captureRepToExp cp) (VarE (mkName "./"))) (VarE (mkName "end")) caps
        captureRepToExp = \case
          MatchRep str -> VarE (mkName "match") `AppE` LitE (StringL str)
          CaptureRep (CodecRep n _ _) -> VarE (mkName "capture") `AppE` VarE n
        qrysE = foldr (\qp -> UInfixE (queryRepToExp qp) (VarE (mkName ".&"))) (VarE (mkName "qend")) qrys
        queryRepToExp (QueryRep idt param) = case param of
          FlagRep -> VarE (mkName "flag") `AppE` lit
          OptionalRep (CodecRep n _ _) -> VarE (mkName "optional") `AppE` lit `AppE` VarE n
          ListRep (CodecRep n _ _) -> VarE (mkName "list") `AppE` lit `AppE` VarE n
          where lit = LitE (StringL idt)
        reqE = case req of
          BodylessRep -> VarE (mkName "bodyless")
          BodyRep (CodecRep n _ _) -> VarE (mkName "body") `AppE` VarE n
        respE = case res of
          CodecRep n _ _ -> VarE (mkName "resp") `AppE` VarE n
        methodE = LitE (StringL method)

trasa :: [RouteRep Name] -> Q [Dec]
trasa routeRepNames = do
  routeReps <- traverse (traverse genCodec) routeRepNames
  route <- routeType routeReps
  cons <- enumRoutesInstance routeReps
  meta <- metaInstance routeReps
  return ([route,cons,meta])

int :: CaptureEncoding Int
int = captureCodecToCaptureEncoding showReadCaptureCodec

bodyInt :: BodyCodec Int
bodyInt = showReadBodyCodec

bodyString :: BodyCodec String
bodyString = showReadBodyCodec

bodyUnit :: BodyCodec ()
bodyUnit = showReadBodyCodec

test :: Q [Dec]
test = trasa [get,post]
  where
    get = RouteRep "Add" "GET"
              [CaptureRep 'int, CaptureRep 'int]
              [QueryRep "int" (OptionalRep 'int)] BodylessRep 'bodyInt
    post = RouteRep "Blog" "POST" [] [QueryRep "id" (OptionalRep 'int)] (BodyRep 'bodyString) 'bodyUnit
