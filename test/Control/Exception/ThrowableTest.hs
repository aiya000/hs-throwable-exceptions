module Control.Exception.ThrowableTest where

import Control.Exception.Safe (Exception)
import Control.Exception.Safe (MonadThrow, throwM, fromException, SomeException)
import Data.Either (isLeft)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, Assertion, assertBool)
import qualified Control.Exception.Throwable as ET


-- The message is never used in tasty-hunit
assertBool' :: Bool -> Assertion
assertBool' = assertBool "" 

canBeShown :: Exception e => e -> Assertion
canBeShown = assertBool' . not . null . show

canBeThrown :: Exception e => e -> Assertion
canBeThrown e = assertBool' . isLeft $ throwM e


test_io_exception_prim :: [TestTree]
test_io_exception_prim =
  [ testCase "can be shown in most cases" $
      canBeShown $ ET.ioException' "honoka-chan!" -- IOException' can be shown, because IOException' is an Exception instance
  , testCase "can be thrown in all case" $
      canBeThrown $ ET.ioException' "kotori-chan ga!" -- IOException' can be thrown by throwM, because  IOException' is an Exception instance !
  ]

test_illegal_argument_exception :: [TestTree]
test_illegal_argument_exception =
  [ testCase "can be shown in most cases" $
      canBeShown $ ET.illegalArgumentException "umi-chan!"
  , testCase "finds the perpetrator with the clue" $
      program -- You can find the perpetrator of some bug if you see the message ('.. is looked like bad')
  ]
  where
    program :: IO ()
    program = do
      case someCalculation of 
        Right _ -> putStrLn "the calculation is succeed"
        Left e ->
          case fromException' e of
            Nothing -> putStrLn "!? I don't know !"
            Just (ET.IllegalArgumentException _ clue) -> putStrLn $ show clue ++ " is looked like bad"

    fromException' :: SomeException -> Maybe (ET.IllegalArgumentException (Int, Int))
    fromException' = fromException

    div' :: MonadThrow m => Int -> Int -> m Int
    x `div'` 0 = throwM $ ET.IllegalArgumentException "0 is not available in div'" (x, 0 :: Int)
    x `div'` y = return $ x `div` y

    someCalculation = 10 `div'` 0
