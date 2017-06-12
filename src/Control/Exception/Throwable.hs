module Control.Exception.Throwable
  ( IOException' (..)
  , ioException'
  , IllegalArgumentException (..)
  , illegalArgumentException
  , GeneralException (..)
  , generalException
  , IndexOutOfBoundsException (..)
  , indexOutOfBoundsException
  ) where

import Control.Exception.Safe (Exception)
import Data.Typeable (Typeable)


-- | Simular to @Control.Exception.IOException@
data IOException' a = IOException' { ioExceptionCause :: String, ioExceptionClue :: a }
                    | FileNotFoundException { fileNotFoundCause :: String, fileNotFoundClue :: a }

instance Show a => Show (IOException' a) where
  show (IOException' cause _) = "IOException': " ++ show cause
  show (FileNotFoundException cause _) = "FileNotFoundException: " ++ show cause

instance (Typeable a, Show a) => Exception (IOException' a)

-- | A constructor for IOException' but doesn't take the clue
ioException' :: String -> IOException' ()
ioException' = flip IOException' ()


-- | Like java.lang.IllegalArgumentException
data IllegalArgumentException a = IllegalArgumentException { illegalArgumentCause :: String, illegalArgumentClue :: a }

instance Show a => Show (IllegalArgumentException a) where
  show (IllegalArgumentException cause  _) = "IllegalArgumentException: " ++ show cause

instance (Typeable a, Show a) => Exception (IllegalArgumentException a)

-- | A constructor for IllegalArgumentException but doesn't take the clue
illegalArgumentException :: String -> IllegalArgumentException ()
illegalArgumentException = flip IllegalArgumentException ()


-- | Like java.lang.IndexOutOfBoundsException
data IndexOutOfBoundsException a = IndexOutOfBoundsException { indexOutOfBoundsCause :: String, indexOutOfBoundsArgumentClue :: a }

instance Show a => Show (IndexOutOfBoundsException a) where
  show (IndexOutOfBoundsException cause  _) = "IndexOutOfBoundsException: " ++ show cause

instance (Typeable a, Show a) => Exception (IndexOutOfBoundsException a)

-- | A constructor for IndexOutOfBoundsException but doesn't take the clue
indexOutOfBoundsException :: String -> IndexOutOfBoundsException ()
indexOutOfBoundsException = flip IndexOutOfBoundsException ()


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
