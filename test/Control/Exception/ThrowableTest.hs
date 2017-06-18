module Control.Exception.ThrowableTest where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Control.Exception.Throwable as ET


test_GeneralException :: [TestTree]
test_GeneralException =
  [ testCase "can be shown" $
      show (ET.generalException "MyTestException" "nico-nico-ni-") @?= "MyTestException: nico-nico-ni-"
  ]
