{-# LANGUAGE TemplateHaskell #-}

-- |
-- The mostly exceptions has the field for its cause, and its clue.
--
-- The cause is the message.
--
-- The clue is some stuff for find bugs,
-- the clue can be omitted if you don't need it.
-- (e.g. @ioException'@, @illegalArgumentException@)
module Control.Exception.Throwable
  ( GeneralException (..)
  , generalException
  , IOException' (..)
  , ioException'
  , IllegalArgumentException (..)
  , illegalArgumentException
  , IndexOutOfBoundsException (..)
  , indexOutOfBoundsException
  ) where

import Control.Exception.Safe (Exception)
import Control.Exception.Throwable.TH (declareException)
import Data.Typeable (Typeable)


-- | An other of these exceptions
data GeneralException a = GeneralException
  { exceptionName  :: String -- ^ This should be named by 'PascalCase' for show, for example: "MyOperation", "Database"
  , exceptionCause :: String -- ^ The message for the cause of the exception
  , exceptionClue  :: a      -- ^ The clue of the exception. This can be anything of Show instance
  }

instance Show a => Show (GeneralException a) where
  show (GeneralException name cause _) = name ++ "Exception: " ++ cause

instance (Typeable a, Show a) => Exception (GeneralException a)

-- | A constructor for GeneralException but doesn't take the clue
generalException :: String -> String -> GeneralException ()
generalException name cause = GeneralException name cause () 


declareException "IOException'"
declareException "IndexOutOfBoundsException"
declareException "IllegalArgumentException"
