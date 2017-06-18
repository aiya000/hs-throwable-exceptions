{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- The data types creator of exceptions in the compile type.
--
-- It has the record of "cause" and "clue".
module Control.Exception.Throwable.TH
  ( declareException
  ) where

import Data.Char (toLower, isUpper, isPunctuation, isNumber)
import Language.Haskell.TH ( Bang(..), DecsQ, newName, mkName
                           , Dec(..), TyVarBndr(..), Con(..), Type(..)
                           , SourceUnpackedness(..), SourceStrictness(..)
                           , Q, Name, Exp(..)
                           , Lit(..), Pat(..), Clause(..), Body(..)
                           )

-- |
-- @
-- data IOException' a = IOException
--   { ioExceptionCause :: String
--   , ioExceptionClue  :: a
--   }
-- ==> do
--   a <- newName "a"
--   ExceptionDataNames { typeName        = mkName "IOException'"
--                      , causeRecordName = mkName "ioExceptionCause"
--                      , clueRecordName  = mkName "ioExceptionClue"
--                      }
-- @
data ExceptionDataNames = ExceptionDataNames
  { typeName        :: String
  , causeRecordName :: String
  , clueRecordName  :: String
  }


-- |
-- Convert string of 'PascalCase' to 'camelCase'.
-- If no 'PascalCase' string or multi byte string is given, undefined behavior is happended.
--
-- >>> pascalToCamelCase "IOException"
-- "ioException"
-- >>> pascalToCamelCase "IOException'"
-- "ioException'"
-- >>> pascalToCamelCase "FOO2BarException"
-- "foO2BarException"
-- >>> pascalToCamelCase "I2SException"
-- "i2SException"
-- >>> pascalToCamelCase "Foo2BarException"
-- "foo2BarException"
-- >>> pascalToCamelCase "IndexOutOfBoundsException"
-- "indexOutOfBoundsException"
-- >>> pascalToCamelCase "IllegalArgumentException'"
-- "illegalArgumentException'"
-- >>> pascalToCamelCase "Exception1"
-- "exception1"
pascalToCamelCase :: String -> String
pascalToCamelCase pascalCase
  | upperIsCont pascalCase  = forUpperContCase pascalCase  -- e.g. "IOException'", "FOOBarException", "FOO2BarException"
  | signInMiddle pascalCase = forSignMiddleCase pascalCase -- e.g. "I2SException", "Foo2BarException" ("Exception1" is not)
  | otherwise               = forCasualCase pascalCase     -- e.g. "IndexOutOfBoundsException", "Exception1",
  where
    -- Are the upper characters continuing ?
    upperIsCont :: String -> Bool
    upperIsCont ""      = False
    upperIsCont (_:a:_) = isUpper a
    upperIsCont (_:_)   = False

    -- Is the sign put in the middle ?
    signInMiddle :: String -> Bool
    signInMiddle = any isNumber . init . tail

    forUpperContCase :: String -> String
    forUpperContCase pascalCase =                                      -- IOException'
      let (pascalCase', signs) = span (not . isPunctuation) pascalCase -- ("IOException", "'")
          (p:ascalCase)        = pascalCase'                           -- ('I':"OException)
          p'                   = toLower p                             -- 'i'
          (ascal, case_)       = span isUpper ascalCase                -- ("OE", "xception")
          (l:acsa)             = reverse ascal                         -- ('E':"O")
          asca                 = map toLower $ reverse acsa            -- "o"
      in p':asca ++ l:case_ ++ signs                                   -- ('i':"o") ++ ('E':xception) ++ "'" == "ioException'" ;P

    forSignMiddleCase :: String -> String
    forSignMiddleCase pascal0Case =
      let (pascal, _0Case) = span (not . isNumber) pascal0Case
          pascal'          = map toLower pascal
      in pascal' ++ _0Case

    forCasualCase :: String -> String
    forCasualCase pascalCase =
      let (p:ascalCase) = pascalCase
          p'            = toLower p
      in p' : ascalCase


-- |
-- Declare simple concrete exception datat type in the compile time.
--
-- @exceptionName@ must be PascalCase
-- (e.g> "IOException'", "IndexOutOfBoundsException". NG> "ioException'", "indexOutOfBoundsException") .
--
-- And @exceptionName@ should have the suffix of "Exception".
declareException :: String -> DecsQ
declareException exceptionName = do
  typeParam <- newName "a"
  let typeNames   = getTypeNames exceptionName
      dataDec     = defineDatatype typeNames typeParam
  showInstanceDec     <- defineShowInstanceFor typeNames
  exceptionInstancDec <- defineExceptionInstanceFor typeNames
  fakeConstructorDec  <- defineFakeConstructorFor typeNames
  return [ dataDec
         , showInstanceDec
         , exceptionInstancDec
         , fakeConstructorDec
         ]
  where
    -- The natural strategy of the evaluation
    noBang :: Bang
    noBang = Bang NoSourceUnpackedness NoSourceStrictness

    -- Create the value of @ExceptionDataNames@ by the type name.
    -- That type name will be executed the instantiation for @Exception@.
    getTypeNames :: String -> ExceptionDataNames
    getTypeNames exceptionName =
      let (e:xceptionName)   = exceptionName  --TODO: Guard empty pattern
          camelExceptionName = (toLower e) : xceptionName
      in ExceptionDataNames { typeName        = exceptionName
                            , causeRecordName = camelExceptionName ++ "Cause"
                            , clueRecordName  = camelExceptionName ++ "Clue"
                            }

    -- Define a data of an exception.
    -- the data is defined by @ExceptionDataNames@.
    defineDatatype :: ExceptionDataNames -> Name -> Dec
    defineDatatype (ExceptionDataNames {..}) a =
      let exception   = mkName typeName
          causeRecord = mkName causeRecordName
          clueRecord  = mkName clueRecordName
      in DataD []
        exception [PlainTV a] Nothing
        [ RecC exception [ (causeRecord, noBang, ConT $ mkName "String")
                         , (clueRecord, noBang, VarT a)
                         ]
        ] []

    -- Define an instance of a data of @ExceptionDataNames@ for @Show@.
    defineShowInstanceFor :: ExceptionDataNames -> Q Dec
    defineShowInstanceFor exceptionDataNames@(ExceptionDataNames {..}) = do
      let showClass = mkName "Show"
          exception = mkName typeName
      a <- newName "a"
      showImpl <- declareShowFunc exceptionDataNames
      return $ InstanceD Nothing
        [AppT (ConT showClass) (VarT a)]
        (AppT (ConT showClass) (AppT (ConT exception) (VarT a)))
        [showImpl]

    -- Define an instance of a data of @ExceptionDataNames@ for @Exception@.
    defineExceptionInstanceFor :: ExceptionDataNames -> Q Dec
    defineExceptionInstanceFor exceptionDataNames@(ExceptionDataNames {..}) = do
      let typeableClass  = mkName "Typeable"
          showClass      = mkName "Show"
          exceptionClass = mkName "Exception"
          exception      = mkName typeName
      a <- newName "a"
      return $ InstanceD Nothing
        [ AppT (ConT typeableClass) (VarT a)
        , AppT (ConT showClass) (VarT a)
        ]
        (AppT (ConT exceptionClass) (AppT (ConT exception) (VarT a)))
        []

    -- Define @show@ function implementation for @defineShowInstanceFor@.
    declareShowFunc :: ExceptionDataNames -> Q Dec
    declareShowFunc ExceptionDataNames {..} = do
      let exception = mkName typeName
          showFunc  = mkName "show"
      cause <- newName "cause"
      return $ FunD showFunc [ -- show
          Clause [ConP exception [VarP cause, WildP]] (NormalB -- (FooException cause _) =
            (InfixE (Just . LitE . StringL $ typeName ++ ": ") (VarE $ mkName "++") (Just $ AppE (VarE showFunc) (VarE cause))) -- show ("FooException: " ++ show cause)
          ) []
        ]

    -- Define the casual data constructor.
    -- Like @Control.Exception.Throwable.generalException@ without name field.
    defineFakeConstructorFor :: ExceptionDataNames -> Q Dec
    defineFakeConstructorFor ExceptionDataNames {..} = do
      let fConstructor = mkName $ pascalToCamelCase exceptionName
          exception    = mkName typeName
      a <- newName "a"
      return $ FunD fConstructor [
          Clause [VarP a] (NormalB $ AppE (AppE (ConE exception) (VarE a)) (TupE []))
          []
        ]
