{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed (embedFile)
import Reflex.Dom

app :: MonadWidget t m => m ()
app = return ()

main :: IO ()
main = mainWidgetWithCss $(embedFile "semantic.min.css") app
