{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Exception.Throwable.THTest where

import Control.Exception.Safe (Exception)
import Control.Monad (void)
import Data.Typeable (Typeable)
import Test.DocTest (doctest)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, Assertion)
import qualified Control.Exception.Throwable.TH as ETH

ETH.declareException "THTestException"

-- Indicate `e ()` is Exception instance in the compile time
type IsException e = (Show (e ()), Typeable (e ()), Exception (e ())) => e ()


refl :: IsException a
refl = undefined

through :: a -> Assertion
through = void . return


test_doctest :: [TestTree]
test_doctest =
  [ testCase "can through TH.hs" $ doctest ["src/Control/Exception/Throwable/TH.hs"]
  ]

test_generated_datatype :: [TestTree]
test_generated_datatype =
  [ testCase "is Exception instance" $ through (refl :: IsException THTestException)
  ]
