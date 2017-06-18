# :diamonds: throwable-exceptions :diamonds:
[![Build Status](https://travis-ci.org/aiya000/hs-throwable-exceptions.svg?branch=master)](https://travis-ci.org/aiya000/hs-throwable-exceptions)
[![Hackage](https://img.shields.io/hackage/v/lens.svg)](https://hackage.haskell.org/package/throwable-exceptions)

throwable-exceptions gives the easy way to create the data types of `Exception` instance with TemplateHaskell,
and gives the simple data types of `Exception` instance with its value constructor,
for your haskell project :dog:

- `throwable-exceptions` is available in
    - [Hackage](https://hackage.haskell.org/package/throwable-exceptions)
    - [stackage (nightly build)](https://www.stackage.org/nightly-2017-06-18/package/throwable-exceptions)


## :books: Document is available in here :books:

- [throwable-exceptions - Hackage](https://hackage.haskell.org/package/throwable-exceptions)


# :muscle: Why we should use this ? :muscle:
We want to throw some exception frequently, but the mostly throwable exceptions are not given by base.  
throwable-exceptions complements it :+1:


## Examples

- vvv  The summary of the exact examples is available here  vvv
    - [example/Main.hs](https://github.com/aiya000/hs-throwable-exceptions/blob/master/example/Main.hs)

- - -

You can create a data type of `Exception` instance by **a line** :exclamation:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Main where

declareException "MyException"
-- ^^^
-- This is same to write below line yourself
--     data MyException a = MyException
--      { myExceptionCause :: String
--      , MyExceptionClue  :: a --      } deriving (Show, Typeable)
--     instance (Typeable a, Show a) => Exception (MyException a)

main :: IO ()
main = do
  print $ MyException "hi" 10
  print $ myException "poi"
```

- - -

Several exception is defined by default :smile:

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
