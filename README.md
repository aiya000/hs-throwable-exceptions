# :diamonds: throwable-exceptions :diamonds:
[![Build Status](https://travis-ci.org/aiya000/hs-throwable-exceptions.svg?branch=master)](https://travis-ci.org/aiya000/hs-throwable-exceptions)
[![Hackage](https://img.shields.io/badge/hackage-v0.1.0.x-blue.svg)](https://hackage.haskell.org/package/throwable-exceptions)

`throwable-exceptions` gives an easy way to create the data types of `Exception` instance with [TemplateHaskell](https://wiki.haskell.org/Template_Haskell),
and gives simple data types of `Exception` instance with its value constructor,
for your haskell project :dog:

- `throwable-exceptions` is available in
    - [Hackage](https://hackage.haskell.org/package/throwable-exceptions)
    - [stackage (nightly build)](https://www.stackage.org/nightly-2017-06-18/package/throwable-exceptions)


## :books: Document is available in here :books:

- [throwable-exceptions - Hackage](https://hackage.haskell.org/package/throwable-exceptions)


# :muscle: Why should we use this ? :muscle:
We want to throw some exception frequently, but the mostly throwable exceptions are not given by `base`.  
`throwable-exceptions` complements it :+1:


## Examples

- vvv  The summary of the exact examples is available here  vvv
    - [test/MainTest.hs](https://github.com/aiya000/hs-throwable-exceptions/blob/master/test/MainTest.hs)

- - -

You can create a data type of `Exception` instance by **a line** :exclamation:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Main where

-- This is same as
--
-- data MyException a = MyException
--  { myExceptionCause :: String
--  , myExceptionClue  :: a
--  } deriving (Show, Typeable)
-- instance (Typeable a, Show a) => Exception (MyException a)
--
declareException "MyException" ["MyException"]

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

main :: IO ()
main = do
  print $ ([1..4] `at` 5 :: Either SomeException Int)
  print $ MyException "hi" 10
  print $ myException "poi"
  print $ firstException "chino"
  print $ secondException "cocoa"
```

- the completely example
    - in [example/Main.hs](https://github.com/aiya000/hs-throwable-exceptions/blob/master/example/Main.hs)

- - -

Several exceptions are defined by default :smile:

For example, [IOException](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Exception.html#t:IOException)'s value constructor is not given :cry:  
But you can use `Control.Exception.Throwable.IOException'` instead :dog:

```haskell
module Main where

main :: IO ()
main = do
  throwM $ IOException' "oops!" "in main"
  throwM $ ioException' "oops!"
```


# :+1:
PR is welcome !
