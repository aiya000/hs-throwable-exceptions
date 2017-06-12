module Control.Exception.Throwable
  ( IOException' (..)
  , IllegalArgumentException (..)
  ) where

import Control.Exception.Safe (Exception)


-- | Simular to @Control.Exception.IOException@
data IOException' = IOException' { ioExceptionCause :: String }
                  | FileNotFoundException { fileNotFoundCause :: String }
  deriving (Read, Show)

instance Exception IOException'


-- Like java.lang.IllegalArgumentException
data IllegalArgumentException = IllegalArgumentException { illegalArgumentCause :: String }
  deriving (Read, Show)

instance Exception IllegalArgumentException
