{-# LANGUAGE TemplateHaskell #-}

-- |
-- Basic specs.
-- And this is an example code.
--
-- This should be compiled without the compile error.
module MainTest where

import Control.Exception.Safe (throwM, MonadCatch, catch, SomeException)
import Control.Exception.Throwable (GeneralException(..), generalException, IndexOutOfBoundsException(..))
import Control.Exception.Throwable.TH (declareException)

{- !! These line is needed by declareException !! -}
import Control.Exception.Safe (Exception)
import Data.Typeable (Typeable)
{- ---------------------------------------------- -}

-- For test
import System.IO.Silently (silence)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))


-- This is same as
--
-- data MyException a = MyException
--  { myExceptionCause :: String
--  , myExceptionClue  :: a
--  } deriving (Typeable)
--
-- instance Show a => Show (MyException a) where
--  ...
-- instance (Typeable a, Show a) => Exception (MyException a)
--
declareException "MyException" ["MyException"]
-- ^ the second argument (the list of String) will be made as a value constructor.

-- This is same as
--
-- data TwoException a =
--  FirstException
--    { firstExceptionCause :: String
--    , firstExceptionClue  :: a
--    } |
--  SecondException
--    { secondExceptionCause :: String
--    , secondExceptionClue  :: a
--    } deriving (Typeable)
--
-- instance Show a => (TwoException a) where
--  ...
-- instance (Typeable a, Show a) => Exception (TwoException a)
--
declareException "TwoException" ["FirstException", "SecondException"]


-- A context of IO
main' :: IO ()
main' = do
  print $ ([1..4] `at` 5 :: Either SomeException Int)
  print $ MyException "hi" (10 :: Int)
  print $ myException "poi"
  -- ^ a (fake) value constructor,
  -- this is same as `MyException` but without a clue value.
  print (foo :: Either SomeException Int)
  print $ FirstException "chino" (20 :: Int)
  print $ firstException "cocoa"
  print $ secondException "rize"
--
-- vvv  output result  vvv
--
-- Left IndexOutOfBoundsException: "index is too big"
-- MyException: "hi"
-- MyException: "poi"
-- Left FooException: poi poi poi !?


-- IndexOutOfBoundsException,
-- IOException',
-- IllegalArgumentException,
-- and GeneralException is presented by default.
at :: (MonadCatch m, Typeable a, Show a) => [a] -> Int -> m a
at xs index = do
  if index < length xs
    then return $ xs !! index
    else throwM $ IndexOutOfBoundsException "index is too big" (xs, index)
    --else throwM $ indexOutOfBoundsException' "index is too big"
    -- You can use above code without some clue


-- If you don't want to use TemplateHaskell (Control.Exception.Throwable.TH),
-- you can use GeneralException and generalException instead !
--
-- data GeneralException a = GeneralException
--   { exceptionName  :: String -- ^ This should be named by 'PascalCase' for show, for example: "MyOperation", "Database"
--   , exceptionCause :: String -- ^ The message for the cause of the exception
--   , exceptionClue  :: a      -- ^ The clue of the exception. This can be anything of Show instance
--   }
-- 
-- generalException :: String -> String -> GeneralException ()
-- generalException name cause = GeneralException name cause () 
--
foo :: (MonadCatch m, Typeable a, Show a) => m a
foo = throwM $ generalException "FooException" "poi poi poi !?"
-- This is same as `GeneralException "FooException" "poi poi poi !?" ()`


test_this_code_should_be_compiled_without_the_compile_error :: [TestTree]
test_this_code_should_be_compiled_without_the_compile_error = [testCase "" $ silence main']
