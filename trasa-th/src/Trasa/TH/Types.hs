{-# LANGUAGE DeriveTraversable #-}
module Trasa.TH.Types where

import qualified Data.List.NonEmpty as NE
import Language.Haskell.TH

data CodecRep = CodecRep
  { codecRepName :: Name
  , codecRepCodec :: Type
  , codecRepType :: Type
  } deriving Show

data CaptureRep codecRep
  = MatchRep String
  | CaptureRep codecRep
  deriving (Show,Foldable,Functor,Traversable)

data ParamRep codecRep
  = FlagRep
  | OptionalRep codecRep
  | ListRep codecRep
  deriving (Show,Foldable,Functor,Traversable)

data QueryRep codecRep = QueryRep
  { queryRepKey :: String
  , queryRepParam :: ParamRep codecRep
  } deriving (Show,Foldable,Functor,Traversable)

data RouteRep codecRep = RouteRep
  { routeRepName :: String
  , routeRepMethod :: String
  , routeRepCaptures :: [CaptureRep codecRep]
  , routeRepQueries :: [QueryRep codecRep]
  , routeReqRequest :: [codecRep]
  , routeReqResponse :: NE.NonEmpty codecRep
  } deriving (Show,Foldable,Functor,Traversable)

data RoutesRep codecRep = RoutesRep
  { routesRepName :: String
  , routesRepRoutes :: [RouteRep codecRep]
  } deriving (Show,Foldable,Functor,Traversable)
