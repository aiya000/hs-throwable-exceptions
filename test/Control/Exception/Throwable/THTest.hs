module Control.Exception.Throwable.THTest where

import Test.DocTest (doctest)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, Assertion)
import qualified Control.Exception.Throwable.TH as ETH


test_doctest :: [TestTree]
test_doctest =
  [ testCase "can through TH.hs" $ doctest ["src/Control/Exception/Throwable/TH.hs"]
  ]
