{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed (embedFile)
import Common
import Trasa.Core (metaCodecToMetaClient)
import Trasa.Error
import Trasa.Client
import Network.HTTP.Client

--css :: Text
--css = $(embedFile "semantic.min.css")

scheme :: Scheme
scheme = Http

authority :: Authority
authority = Authority scheme "127.0.0.1" Nothing

config :: IO Config
config = do
  mngr <- newManager defaultManagerSettings 
  pure $ Config authority mempty mngr

client :: IO (Either TrasaErr resp)
client = do
  cfg <- config
  clientWith (metaCodecToMetaClient . meta) cfg _

main :: IO ()
main = pure ()
