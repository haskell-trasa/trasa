{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Trasa.Server
  ( serve
  ) where

import Trasa.Core
import Network.HTTP.Types.Header (hAccept,hContentType)
import Network.HTTP.Types.Status (status400,status200,mkStatus)
import Data.Functor.Identity
import Data.Vinyl.Core (Rec)
import qualified Network.Wai as WAI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.List as L

serve :: 
     (forall cs' rq' rp'. rt cs' rq' rp' -> T.Text) -- ^ Method
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Path CaptureDecoding cs')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> RequestBody (Many BodyDecoding) rq')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> ResponseBody (Many BodyEncoding) rp')
  -> (forall cs' rq' rp'. rt cs' rq' rp' -> Rec Identity cs' -> RequestBody Identity rq' -> IO rp')
  -> [Constructed rt] -- ^ All available routes
  -> WAI.Application -- ^ WAI Application
serve toMethod toCapDec toReqBody toRespBody makeResponse enumeratedRoutes =
  let handle = dispatchWith toMethod toCapDec toReqBody toRespBody makeResponse enumeratedRoutes
   in \req respond -> do
        let mstuff = (,,)
              <$> eitherToMaybe (TE.decodeUtf8' (WAI.requestMethod req))
              <*> (case L.lookup hAccept (WAI.requestHeaders req) of
                     Nothing -> Just []
                     Just acceptsBs -> do
                       accepts <- eitherToMaybe (TE.decodeUtf8' acceptsBs)
                       -- This would be the right way to handle mime types
                       -- mapM (breakRequired (=='/')) (T.split (==',') headers)
                       return (map T.strip (T.splitOn (T.singleton ',') accepts))
                  )
              <*> (case L.lookup hContentType (WAI.requestHeaders req) of
                     Nothing -> Just Nothing
                     Just contentTypeBs -> fmap Just (eitherToMaybe (TE.decodeUtf8' contentTypeBs))
                  )
        case mstuff of
          Nothing -> respond (WAI.responseLBS status400 [] "Malformed content-type, accept headers, or request method")
          Just (method,accepts,mcontentType) -> do
            let path = WAI.pathInfo req
            mcontent <- case mcontentType of
              Nothing -> return Nothing
              Just typ -> do
                reqBod <- WAI.strictRequestBody req
                return (Just (Content typ reqBod))
            e <- handle method accepts path mcontent
            case e of
              Left (TrasaErr errCode phrase errBody) -> do
                let status = mkStatus errCode (TE.encodeUtf8 phrase)
                respond (WAI.responseLBS status [] errBody)
              Right lbs -> respond (WAI.responseLBS status200 [] lbs)
  
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

-- | Would be used to split up mime types once we handle them properly.
-- breakRequired :: (Char -> Bool) -> Text -> Maybe (Text,Text)
-- breakRequired f t = let (a,b) = T.break f t in
--   if T.null b
--     then Nothing
--     else Just (a, T.tail b)

