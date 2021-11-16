module Main (main) where

import Spec.Direct qualified as Direct
import Spec.Integration qualified as Integration
import Spec.Roundtrip qualified as Roundtrip
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Majority Multisign"
    [ Roundtrip.tests
    , Direct.tests
    , Integration.tests
    ]
