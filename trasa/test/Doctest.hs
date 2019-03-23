module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = do
  putStrLn "\nRUNNING DOCTESTS"
  doctest
    [ "src"
    ]
