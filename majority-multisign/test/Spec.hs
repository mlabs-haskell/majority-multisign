module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Majority Multisign"
    []
