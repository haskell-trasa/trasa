{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Trasa.Url
  (
  -- * Untyped Query Parameters
    QueryParam(..)
  , QueryString(..)
  , encodeQuery
  , decodeQuery
  -- * Urls (path + query string)
  , Url(..)
  , encodeUrl
  , decodeUrl
  ) where

import Data.Semigroup (Semigroup(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Types as N

data QueryParam
  = QueryParamFlag
  | QueryParamSingle T.Text
  | QueryParamList [T.Text]
  deriving Eq

instance Semigroup QueryParam where
  QueryParamFlag <> q = q
  q <> QueryParamFlag = q
  QueryParamSingle q1 <> QueryParamSingle q2 = QueryParamList [q1,q2]
  QueryParamSingle q1 <> QueryParamList l1 = QueryParamList (q1:l1)
  QueryParamList l1 <> QueryParamSingle q1 = QueryParamList (l1 ++ [q1]) -- O(n^2)...
  QueryParamList l1 <> QueryParamList l2 = QueryParamList (l1 ++ l2)

instance Monoid QueryParam where
  mempty = QueryParamFlag
  mappend = (<>)

newtype QueryString = QueryString
  { unQueryString :: HM.HashMap T.Text QueryParam
  } deriving Eq

encodeQuery :: QueryString -> N.Query
encodeQuery = HM.foldrWithKey (\key param items -> toQueryItem key param ++ items) [] . unQueryString
  where
    toQueryItem :: T.Text -> QueryParam -> N.Query
    toQueryItem key = \case
      QueryParamFlag -> [(T.encodeUtf8 key, Nothing)]
      QueryParamSingle value -> [(T.encodeUtf8 key, Just (T.encodeUtf8 value))]
      QueryParamList values ->
        flip fmap values $ \value -> (T.encodeUtf8 key, Just (T.encodeUtf8 value))

decodeQuery :: N.Query -> QueryString
decodeQuery = QueryString . HM.fromListWith (<>) . fmap decode
  where
    decode (key,mval) = case mval of
      Nothing  -> (tkey,QueryParamFlag)
      Just val -> (tkey,QueryParamSingle (T.decodeUtf8 val))
      where tkey = T.decodeUtf8 key

data Url = Url
  { urlPath :: ![T.Text]
  , urlQueryString :: !QueryString
  } deriving Eq

instance Show Url where
  show = show . encodeUrl

encodeUrl :: Url -> T.Text
encodeUrl (Url path querys) =
  ( T.decodeUtf8
  . LBS.toStrict
  . LBS.toLazyByteString
  . encode
  . encodeQuery ) querys
  where
    encode qs = case path of
      [] -> "/" <> N.encodePath path qs
      _  -> N.encodePath path qs

decodeUrl :: T.Text -> Url
decodeUrl txt = Url path (decodeQuery querys)
  where (path,querys) = N.decodePath (T.encodeUtf8 txt)
