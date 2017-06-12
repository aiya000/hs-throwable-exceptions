module Control.Exception.Throwable
  ( IOException' (..)
  , ioException'
  , IllegalArgumentException (..)
  , illegalArgumentException
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


-- Like java.lang.IllegalArgumentException
data IllegalArgumentException a = IllegalArgumentException { illegalArgumentCause :: String, illegalArgumentClue :: a }

instance Show a => Show (IllegalArgumentException a) where
  show (IllegalArgumentException cause  _) = "IllegalArgumentException: " ++ show cause

instance (Typeable a, Show a) => Exception (IllegalArgumentException a)

-- | A constructor for IllegalArgumentException but doesn't take the clue
illegalArgumentException :: String -> IllegalArgumentException ()
illegalArgumentException = flip IllegalArgumentException ()


