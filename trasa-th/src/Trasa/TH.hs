{-# LANGUAGE CPP             #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Trasa.TH
  (
    Name
  , CaptureRep(..)
  , ParamRep(..)
  , QueryRep(..)
  , RouteRep(..)
  , RoutesRep(..)
  , routeDataType
  , enumRoutesInstance
  , metaInstance
  , trasa
  , parseTrasa
  )where

import           Data.Kind                 (Type)
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe                (listToMaybe, mapMaybe)
import           Language.Haskell.TH       hiding (Type, match)
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote
import           Trasa.Core
import           Trasa.Core.Implicit

import           Trasa.TH.Parse            (parseRoutesRep)
import           Trasa.TH.Types

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
  kind <- [t| [Type] -> [Param] -> Bodiedness -> Type -> Type |]
  return (DataD [] route [] (Just kind) (fmap (buildCons route) routeReps) [])
  where
    route = mkName routeStr
    buildCons :: Name -> RouteRep CodecRep -> Con
    buildCons rt (RouteRep name _ captures queries request response) = GadtC [mkName name] [] typ
      where
       typ =
        ConT rt `AppT`
        typeList (mapMaybe captureType captures) `AppT`
        typeList (fmap (paramType . queryRepParam) queries) `AppT`
        bodiednessType request `AppT`
        responseType response
    captureType :: CaptureRep CodecRep -> Maybe TH.Type
    captureType = \case
      MatchRep _ -> Nothing
      CaptureRep (CodecRep _ _ typ) -> Just typ
    paramType :: ParamRep CodecRep -> TH.Type
    paramType = \case
      FlagRep -> PromotedT 'Flag
      OptionalRep (CodecRep _ _ typ) -> PromotedT 'Optional `AppT` typ
      ListRep (CodecRep _ _ typ) -> PromotedT 'List `AppT` typ
    bodiednessType :: [CodecRep] -> TH.Type
    bodiednessType = \case
      [] -> PromotedT 'Bodyless
      (CodecRep _ _ typ:_) -> PromotedT 'Body `AppT` typ
    responseType = \case
      (CodecRep _ _ typ NE.:| _) -> typ

routeDataType :: RoutesRep Name -> Q Dec
routeDataType = genCodecs routeDataTypeCodec

enumRoutesInstanceCodec :: RoutesRep CodecRep -> Dec
enumRoutesInstanceCodec (RoutesRep routeStr routeReps) =
  InstanceD Nothing [] typ [FunD 'enumerateRoutes [Clause [] (NormalB (ListE expr)) []]]
  where
    route = mkName routeStr
    typ = ConT ''EnumerableRoute `AppT` ConT route
    buildCons name = ConE 'Constructed `AppE` ConE (mkName name)
    expr = fmap (buildCons . routeRepName) routeReps

enumRoutesInstance :: RoutesRep Name -> Q Dec
enumRoutesInstance = genCodecs (return . enumRoutesInstanceCodec)

metaInstanceCodec :: RoutesRep CodecRep -> Q Dec
metaInstanceCodec (RoutesRep routeStr routeReps) = do
  let route = mkName routeStr
      typ = ConT ''HasMeta `AppT` ConT route
  capStrat <- search routeRepCaptures capCodec [t| CaptureCodec |]
  qryStrat <- search routeRepQueries (paramCodec . queryRepParam) [t| CaptureCodec |]
  reqBodyStrat <- search routeReqRequest (Just . codecRepCodec)  [t| BodyCodec |]
  respBodyStrat <- search (NE.toList . routeReqResponse) (Just . codecRepCodec) [t| BodyCodec |]
  many <- [t| Many |]
#if !MIN_VERSION_template_haskell(2,15,0)
  let mkTypeFamily str strat = TySynInstD (mkName str) (TySynEqn [ConT route] strat)
#else
  let mkTypeFamily str strat = TySynInstD (TySynEqn (Just [PlainTV (mkName str) ()]) (ConT route) strat)
#endif
      typeFamilies =
        [ mkTypeFamily "CaptureStrategy" capStrat
        , mkTypeFamily "QueryStrategy" qryStrat
        , mkTypeFamily "RequestBodyStrategy" (many `AppT` reqBodyStrat)
        , mkTypeFamily "ResponseBodyStrategy" (many `AppT` respBodyStrat)
        ]
  lam <- newName "route"
  let metaExp = LamE [VarP lam] (CaseE (VarE lam) (fmap routeRepToMetaPattern routeReps))
      metaFunction = FunD 'meta [Clause [] (NormalB metaExp) []]
  return (InstanceD Nothing [] typ (typeFamilies ++ [metaFunction]))
  where
    search :: (RouteRep CodecRep -> [b]) -> (b -> Maybe TH.Type) -> Q TH.Type -> Q TH.Type
    search f g err = case listToMaybe (mapMaybe g (routeReps >>= f)) of
      Just t  -> return t
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
    routeRepToMetaPattern :: RouteRep CodecRep -> Match
    routeRepToMetaPattern (RouteRep name method caps qrys req res) =
      Match (ConP (mkName name) []) (NormalB expr) []
      where
        expr =
          ConE 'Meta `AppE`
          capsE `AppE`
          qrysE `AppE`
          reqE `AppE`
          respE `AppE`
          methodE
        capsE = foldr (\cp -> UInfixE (captureRepToExp cp) (VarE '(./))) (VarE 'end) caps
        captureRepToExp = \case
          MatchRep str -> VarE 'match `AppE` LitE (StringL str)
          CaptureRep (CodecRep n _ _) -> VarE 'capture `AppE` VarE n
        qrysE = foldr (\qp -> UInfixE (queryRepToExp qp) (VarE '(.&))) (VarE 'qend) qrys
        queryRepToExp (QueryRep idt param) = case param of
          FlagRep -> VarE 'flag `AppE` lit
          OptionalRep (CodecRep n _ _) -> VarE 'optional `AppE` lit `AppE` VarE n
          ListRep (CodecRep n _ _) -> VarE 'list `AppE` lit `AppE` VarE n
          where lit = LitE (StringL idt)
        reqE = case req of
          [] -> VarE 'bodyless
          (r : rs) -> VarE 'body `AppE` manyE (r NE.:| rs)
        respE = VarE 'resp `AppE` manyE res
        methodE = LitE (StringL method)
        manyE (CodecRep n _ _ NE.:| xs) =
          ConE 'Many `AppE` (UInfixE (VarE n) (ConE '(NE.:|)) (ListE (VarE . codecRepName <$> xs)))

metaInstance :: RoutesRep Name -> Q Dec
metaInstance = genCodecs metaInstanceCodec

trasa :: RoutesRep Name -> Q [Dec]
trasa routeRepNames = do
  routeReps <- traverse genCodec routeRepNames
  rt <- routeDataTypeCodec routeReps
  let cons = enumRoutesInstanceCodec routeReps
  m <- metaInstanceCodec routeReps
  return [rt, cons, m]

parseTrasa :: QuasiQuoter
parseTrasa = QuasiQuoter err err err quoter
  where
    err _ = fail "parseTrasa: This quasi quoter should only be used on the top level"
    quoter :: String -> Q [Dec]
    quoter str = case parseRoutesRep str of
      Left e              -> fail e
      Right routeRepNames -> trasa routeRepNames
