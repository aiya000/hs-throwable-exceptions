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
import Language.Haskell.TH (Bang(..), DecsQ, newName, mkName, Dec(..), TyVarBndr(..), Con(..), Type(..), SourceUnpackedness(..), SourceStrictness(..), Q, Name, Exp(..), Lit(..), Pat(..), Clause(..), Body(..))

-- |
-- Mean names of a data type and its value constructors
--
-- @
-- data IOException' a =
--   IOException'
--     { ioExceptionCause :: String
--     , ioExceptionClue  :: a
--     } |
--   FileNotFoundException
--     { fileNotFoundExceptionCause :: String
--     , fileNotFoundExceptionClue  :: a
--     }
-- ==>
--   DatatypeNames
--     { typeName     = mkName "IOException'"
--     , constructors =
--       [ ValueConstructor
--         { constrName      = "IOException'"
--         , causeRecordName = mkName "ioExceptionCause"
--         , clueRecordName  = mkName "ioExceptionClue"
--         }
--       , ValueConstructor
--         { constrName      = "FileNotFoundException"
--         , causeRecordName = mkName "fileNotFoundExceptionCause"
--         , clueRecordName  = mkName "fileNotFoundExceptionClue"
--         }
--       ]
--     }
-- @
data DatatypeNames = DatatypeNames
  { datatypeName :: DatatypeName
  , constructors :: [ValueConstructor]
  } deriving (Show)

-- | Mean a name of a value constructor and its records
data ValueConstructor = ValueConstructor
  { constructorName :: ValueConstructorName
  , causeRecordName :: String
  , clueRecordName  :: String
  } deriving (Show)

type DatatypeName = String
type ValueConstructorName = String


-- |
-- Create the value of @DatatypeNames@ by the type name.
-- That type name will be executed the instantiation for @Exception@.
--
-- @typeName@ and @constrNames@ elements must be non empty string.
-- If it is given, create Nothing.
getDatatypeNames :: DatatypeName -> [ValueConstructorName] -> Maybe DatatypeNames
getDatatypeNames "" _                 = Nothing
getDatatypeNames typeName constrNames = fmap (DatatypeNames typeName) $ mapM toValueConstructorName constrNames
  where
    toValueConstructorName :: ValueConstructorName -> Maybe ValueConstructor
    toValueConstructorName [] = Nothing
    toValueConstructorName constrName =
      let constrName' = pascalToCamelCase constrName
      in Just ValueConstructor { constructorName = constrName
                               , causeRecordName = causeRecordName constrName'
                               , clueRecordName  = clueRecordName constrName'
                               }

    -- "ioException'" -> ("ioException", "'")
    splitBodyAndSigns :: String -> (String, String)
    splitBodyAndSigns = span $ not . isPunctuation

    -- recordName "Cause" "ioException'" -> "ioExceptionCause'",
    -- signs is important.
    recordName :: String -> String -> String
    recordName suffix constrName' =
      let (name, signs) = splitBodyAndSigns constrName'
      in name ++ suffix ++ signs

    causeRecordName = recordName "Cause"
    clueRecordName  = recordName "Clue"


-- | A natural strategy of the evaluation
noBang :: Bang
noBang = Bang NoSourceUnpackedness NoSourceStrictness


-- |
-- Declare simple concrete exception data type in the compile time.
--
-- If the empty list is given to @constrNames@, create empty data type.
--
-- @typeName@ and @constrNames@ must be PascalCase
-- (e.g> "IOException'", "IndexOutOfBoundsException". NG> "ioException'", "indexOutOfBoundsException") .
declareException :: DatatypeName -> [ValueConstructorName] -> DecsQ
declareException typeName constrNames = do
  case getDatatypeNames typeName constrNames of
    Nothing        -> fail "Data.Exception.Throwable.TH.declareException requires non empty string for `typeName`"
    Just typeNames -> do
      typeParam <- newName "a" -- type `a` of `data FooException a`
      let dataDec = defineDatatype typeNames typeParam
      showInstanceDec     <- defineShowInstanceFor typeNames
      exceptionInstancDec <- defineExceptionInstanceFor typeNames
      fakeconstructorDecs <- concat <$> mapM (defineFakeConstructor $ datatypeName typeNames) (constructors typeNames)
      return $ [ dataDec
               , showInstanceDec
               , exceptionInstancDec
               ] ++ fakeconstructorDecs
  where
    -- Define a data of an exception.
    -- the data is defined by @DatatypeNames@.
    defineDatatype :: DatatypeNames -> Name -> Dec
    defineDatatype (DatatypeNames {..}) a =
      let exception   = mkName datatypeName
          constrsCons = map (flip makeValueConstructorsCon a) constructors
      in DataD []
        exception [PlainTV a] Nothing
        constrsCons []

    -- Make @Con@ from @ValueConstructor@ for @defineDatatype@
    -- @a@ is @defineDatatype@'s datatype's type parameter.
    makeValueConstructorsCon :: ValueConstructor -> Name -> Con
    makeValueConstructorsCon (ValueConstructor {..}) a =
      let constructor = mkName constructorName
          causeRecord = mkName causeRecordName
          clueRecord  = mkName clueRecordName
      in RecC constructor [ (causeRecord, noBang, ConT $ mkName "String")
                          , (clueRecord, noBang, VarT a)
                          ]

    -- Define an instance of a data of @DatatypeNames@ for @Show@.
    defineShowInstanceFor :: DatatypeNames -> Q Dec
    defineShowInstanceFor dataTypeNames@(DatatypeNames {..}) = do
      let showClass = mkName "Show"
          exception = mkName typeName
      a <- newName "a"
      showFuncDec <- declareShowFunc dataTypeNames
      return $ InstanceD Nothing
        [AppT (ConT showClass) (VarT a)]
        (AppT (ConT showClass) (AppT (ConT exception) (VarT a)))
        [showFuncDec]

    -- Make a @show@ function definition.
    declareShowFunc :: DatatypeNames -> Q Dec
    declareShowFunc DatatypeNames {..} = do
      let showFunc = mkName "show"
      showFuncClauses <- mapM (flip makeShowFuncClause showFunc) constructors
      return $ FunD showFunc showFuncClauses
      where
        -- Make patterns of @show@ function (@showFunc@) implementation,
        -- for @constructors@ of @DatatypeNames@.
        makeShowFuncClause :: ValueConstructor -> Name -> Q Clause
        makeShowFuncClause (ValueConstructor {..}) showFunc = do
          let constructor = mkName constructorName
          cause <- newName "cause"
          return $ Clause [ConP constructor [VarP cause, WildP]] -- (FooException cause _) =
            (NormalB
                (InfixE (Just . LitE . StringL $ typeName ++ ": ") (VarE $ mkName "++") (Just $ AppE (VarE showFunc) (VarE cause))) -- show ("FooException: " ++ show cause)
            ) []

    -- Define an instance of a data of @DatatypeNames@ for @Exception@.
    defineExceptionInstanceFor :: DatatypeNames -> Q Dec
    defineExceptionInstanceFor DatatypeNames {..} = do
      let typeableClass  = mkName "Typeable"
          showClass      = mkName "Show"
          exceptionClass = mkName "Exception"
          exception      = mkName typeName
      a <- newName "a"
      return $ InstanceD Nothing
        [ ConT typeableClass `AppT` VarT a
        , ConT showClass `AppT` VarT a
        ]
        (ConT exceptionClass `AppT` ParensT (ConT exception `AppT` VarT a))
        []

    -- Define the casual data constructor.
    -- Like @Control.Exception.Throwable.generalException@ without name field.
    --
    -- Take a name of the target value constructor, and a name of its data type.
    --
    -- Why @DecsQ@ is returned, because splicing type signature should be avoided.
    defineFakeConstructor :: String -> ValueConstructor -> DecsQ
    defineFakeConstructor typeName (ValueConstructor {..}) = do
      let datatype     = mkName typeName
          fConstructor = mkName $ pascalToCamelCase constructorName
          constructor  = mkName constructorName
      x <- newName "x"
      let sig  = SigD fConstructor $ ArrowT `AppT` ConT (mkName "String") `AppT` (ConT datatype `AppT` TupleT 0)
      let impl = FunD fConstructor [
                   Clause [VarP x] (NormalB $ ConE constructor `AppE` VarE x `AppE` TupE [])
                    []
                 ]
      return [sig, impl]


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
