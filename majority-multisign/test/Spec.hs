module Main (main) where

import Spec.Direct qualified as Direct
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Majority Multisign"
    [ Direct.tests
    ]
