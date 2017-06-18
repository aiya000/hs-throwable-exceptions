module Control.Exception.ThrowableTest where

import Control.Exception.Safe (Exception)
import Control.Exception.Safe (MonadThrow, throwM, fromException, SomeException)
import Data.Either (isLeft)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, Assertion, assertBool, (@?=))
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
      canBeThrown $ ET.ioException' "kotori-chan ga!" -- IOException' can be thrown by throwM, because IOException' is an Exception instance !
  ]


test_index_out_of_bounds_exception :: [TestTree]
test_index_out_of_bounds_exception =
  [ testCase "finds the perpetrator with the clue" $
      program -- You can find the perpetrator of some bug if you see the message ('.. looks like bad')
  ]
  where
    program :: IO ()
    program = do
      case someCalculation of 
        Right _ -> putStrLn "the calculation is succeed"
        Left e -> case fromException' e of
          Nothing -> putStrLn "!? I don't know !"
          Just (ET.IndexOutOfBoundsException _ clue) -> putStrLn $ show clue ++ " looks like bad"

    -- monomorphic function
    fromException' :: SomeException -> Maybe (ET.IndexOutOfBoundsException ([Int], Int))
    fromException' = fromException

    at :: MonadThrow m => [Int] -> Int -> m Int
    xs `at` i | length xs <= i = throwM $ ET.IndexOutOfBoundsException "mmm..?" (xs, i)
              | otherwise      = return $ xs !! i

    someCalculation = [10..13] `at` 4


test_general_exception :: [TestTree]
test_general_exception =
  [ testCase "looks like other exception when GeneralException is executed `show`" $
      show (ET.generalException "MyTestException" "nico-chan")  @?= "MyTestException: nico-chan"
  ]
