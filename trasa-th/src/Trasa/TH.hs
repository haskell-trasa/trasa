{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Trasa.TH
  (
    Name
  , CaptureRep(..)
  , ParamRep(..)
  , QueryRep(..)
  , BodiednessRep(..)
  , RouteRep(..)
  , RoutesRep(..)
  , routeDataType
  , enumRoutesInstance
  , metaInstance
  , trasa
  , parseTrasa
  )where

import Data.Kind (Type)
import Data.Maybe (listToMaybe,mapMaybe)
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH as TH
import qualified Text.Megaparsec as MP
import Trasa.Core

import Trasa.TH.Types
import Trasa.TH.Parse (parseRouteReps)

genCodec :: Name -> Q CodecRep
genCodec name = reify name >>= \case
  VarI fullName fullType _ -> case fullType of
    AppT codec typ -> return (CodecRep fullName codec typ)
    _ -> fail ("Codec: " ++ show name ++ " does not have a type like (Codec Type) but has the type: " ++ show fullType)
  _ -> fail ("Codec: " ++ show name ++ " is not a haskell value")

genCodecs :: (RoutesRep CodecRep -> Q b) -> RoutesRep Name -> Q b
genCodecs f routeRepNames = do
  routeReps <- traverse genCodec routeRepNames
  f routeReps

typeList :: [TH.Type] -> TH.Type
typeList = foldr (\typ rest -> PromotedConsT `AppT` typ `AppT` rest) PromotedNilT

routeDataTypeCodec :: RoutesRep CodecRep -> Q Dec
routeDataTypeCodec (RoutesRep routeStr routeReps) = do
  let route = mkName routeStr
  kind <- [t| [Type] -> [Param] -> Bodiedness -> Type -> Type |]
  return (DataD [] route [] (Just kind) (fmap (buildCons route) routeReps) [])
  where
    buildCons :: Name -> RouteRep CodecRep -> Con
    buildCons rt (RouteRep name _ captures queries request response) = GadtC [mkName name] [] typ
      where
       typ =
        ConT rt `AppT`
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

routeDataType :: RoutesRep Name -> Q Dec
routeDataType = genCodecs routeDataTypeCodec

enumRoutesInstanceCodec :: RoutesRep CodecRep -> Q Dec
enumRoutesInstanceCodec (RoutesRep routeStr routeReps) = do
  let route = mkName routeStr
  constructedData <- [| Constructed |]
  let typ = ConT (mkName "EnumerableRoute") `AppT` ConT route
      enumRoutes = mkName "enumerateRoutes"
      buildCons name = constructedData `AppE` ConE (mkName name)
      expr = fmap (buildCons . routeRepName) routeReps
  return (InstanceD Nothing [] typ [FunD enumRoutes [Clause [] (NormalB (ListE expr)) []]])

enumRoutesInstance :: RoutesRep Name -> Q Dec
enumRoutesInstance = genCodecs enumRoutesInstanceCodec

metaInstanceCodec :: RoutesRep CodecRep -> Q Dec
metaInstanceCodec (RoutesRep routeStr routeReps) = do
  let route = mkName routeStr
      typ = ConT (mkName "HasMeta") `AppT` ConT route
  capStrat <- search routeRepCaptures capCodec [t| CaptureCodec |]
  qryStrat <- search routeRepQueries (paramCodec . queryRepParam) [t| CaptureCodec |]
  reqBodyStrat <- search ((:[]) .routeReqRequest) bCodec [t| BodyCodec |]
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
    capCodec :: CaptureRep CodecRep -> Maybe TH.Type
    capCodec = \case
      MatchRep _ -> Nothing
      CaptureRep (CodecRep _ codec _) -> Just codec
    paramCodec :: ParamRep CodecRep -> Maybe TH.Type
    paramCodec = \case
      FlagRep -> Nothing
      OptionalRep (CodecRep _ codec _) -> Just codec
      ListRep (CodecRep _ codec _) -> Just codec
    bCodec :: BodiednessRep CodecRep -> Maybe TH.Type
    bCodec = \case
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

metaInstance :: RoutesRep Name -> Q Dec
metaInstance = genCodecs metaInstanceCodec

trasa :: RoutesRep Name -> Q [Dec]
trasa routeRepNames = do
  routeReps <- traverse genCodec routeRepNames
  rt <- routeDataTypeCodec routeReps
  cons <- enumRoutesInstanceCodec routeReps
  m <- metaInstanceCodec routeReps
  return ([rt,cons,m])

parseTrasa :: QuasiQuoter
parseTrasa = QuasiQuoter err err err quoter
  where
    err _ = fail "parseTrasa: This quasi quoter should only be used on the top level"
    quoter :: String -> Q [Dec]
    quoter str = case MP.parse parseRouteReps "" str of
      Left e -> fail (MP.parseErrorPretty e)
      Right routeRepNames -> trasa routeRepNames
