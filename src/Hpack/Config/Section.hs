{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hpack.Config.Section where

import           Data.Aeson.Types
import qualified Data.HashMap.Lazy as HashMap
import           Data.List.Compat ((\\), isPrefixOf)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Hpack.Config.BuildType
import           Hpack.Config.CustomSetup
import           Hpack.Config.CustomSetupSection
import           Hpack.Config.Dependency
import           Hpack.Config.Executable
import           Hpack.Config.Flag
import           Hpack.Config.FlagSection
import           Hpack.Config.GenericParseJson
import           Hpack.Config.HasFieldNames
import           Hpack.Config.Library
import           Hpack.Config.SourceRepository
import           Hpack.Util

data Conditional = Conditional {
  conditionalCondition :: String
, conditionalThen :: Section ()
, conditionalElse :: Maybe (Section ())
} deriving (Eq, Show)

data Package = Package {
  packageName             :: String
, packageVersion          :: String
, packageSynopsis         :: Maybe String
, packageDescription      :: Maybe String
, packageHomepage         :: Maybe String
, packageBugReports       :: Maybe String
, packageCategory         :: Maybe String
, packageStability        :: Maybe String
, packageAuthor           :: [String]
, packageMaintainer       :: [String]
, packageCopyright        :: [String]
, packageBuildType        :: BuildType
, packageLicense          :: Maybe String
, packageLicenseFile      :: [FilePath]
, packageTestedWith       :: Maybe String
, packageFlags            :: [Flag]
, packageExtraSourceFiles :: [FilePath]
, packageDataFiles        :: [FilePath]
, packageSourceRepository :: Maybe SourceRepository
, packageCustomSetup      :: Maybe CustomSetup
, packageLibrary          :: Maybe (Section Library)
, packageExecutables      :: [Section Executable]
, packageTests            :: [Section Executable]
, packageBenchmarks       :: [Section Executable]
} deriving (Eq, Show)

data Section a = Section {
  sectionData               :: a
, sectionSourceDirs         :: [FilePath]
, sectionDependencies       :: [Dependency]
, sectionDefaultExtensions  :: [String]
, sectionOtherExtensions    :: [String]
, sectionGhcOptions         :: [GhcOption]
, sectionGhcProfOptions     :: [GhcProfOption]
, sectionGhcjsOptions       :: [GhcjsOption]
, sectionCppOptions         :: [CppOption]
, sectionCcOptions          :: [CcOption]
, sectionCSources           :: [FilePath]
, sectionJsSources          :: [FilePath]
, sectionExtraLibDirs       :: [FilePath]
, sectionExtraLibraries     :: [FilePath]
, sectionIncludeDirs        :: [FilePath]
, sectionInstallIncludes    :: [FilePath]
, sectionLdOptions          :: [LdOption]
, sectionBuildable          :: Maybe Bool
, sectionConditionals       :: [Conditional]
, sectionBuildTools         :: [Dependency]
} deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasFieldNames a => HasFieldNames (Section a) where
  fieldNames Proxy = fieldNames (Proxy :: Proxy a) ++ fieldNames (Proxy :: Proxy CommonOptions)
  ignoreUnderscoredUnknownFields _ = ignoreUnderscoredUnknownFields (Proxy :: Proxy a)


data CaptureUnknownFields a = CaptureUnknownFields {
  captureUnknownFieldsFields :: [FieldName]
, captureUnknownFieldsValue :: a
} deriving (Eq, Show, Generic)

instance (HasFieldNames a, FromJSON a) => FromJSON (CaptureUnknownFields (Section a)) where
  parseJSON v = do
    (unknownFields, sect) <- toSection <$> parseJSON v <*> parseJSON v
    return (CaptureUnknownFields (unknownSectionFields ++ unknownFields) sect)
    where
      unknownSectionFields = getUnknownFields v (Proxy :: Proxy (Section a))

toConditional :: ConditionalSection -> ([FieldName], Conditional)
toConditional x = case x of
  ThenElseConditional (CaptureUnknownFields fields (ThenElse condition (CaptureUnknownFields fieldsThen then_) (CaptureUnknownFields fieldsElse else_))) ->
      (fields ++ fieldsThen ++ fieldsElse, Conditional condition (() <$ then_) (Just (() <$ else_)))
  FlatConditional (CaptureUnknownFields fields sect) -> (fields, Conditional (conditionCondition $ sectionData sect) (() <$ sect) Nothing)

toSection :: a -> CommonOptions -> ([FieldName], Section a)
toSection a CommonOptions{..}
  = ( concat unknownFields
    , Section {
        sectionData = a
      , sectionSourceDirs         = fromMaybeList commonOptionsSourceDirs
      , sectionDefaultExtensions  = fromMaybeList commonOptionsDefaultExtensions
      , sectionOtherExtensions    = fromMaybeList commonOptionsOtherExtensions
      , sectionGhcOptions         = fromMaybeList commonOptionsGhcOptions
      , sectionGhcProfOptions     = fromMaybeList commonOptionsGhcProfOptions
      , sectionGhcjsOptions       = fromMaybeList commonOptionsGhcjsOptions
      , sectionCppOptions         = fromMaybeList commonOptionsCppOptions
      , sectionCcOptions          = fromMaybeList commonOptionsCcOptions
      , sectionCSources           = fromMaybeList commonOptionsCSources
      , sectionJsSources          = fromMaybeList commonOptionsJsSources
      , sectionExtraLibDirs       = fromMaybeList commonOptionsExtraLibDirs
      , sectionExtraLibraries     = fromMaybeList commonOptionsExtraLibraries
      , sectionIncludeDirs        = fromMaybeList commonOptionsIncludeDirs
      , sectionInstallIncludes    = fromMaybeList commonOptionsInstallIncludes
      , sectionLdOptions          = fromMaybeList commonOptionsLdOptions
      , sectionBuildable          = commonOptionsBuildable
      , sectionDependencies       = fromMaybeList commonOptionsDependencies
      , sectionConditionals       = conditionals
      , sectionBuildTools         = fromMaybeList commonOptionsBuildTools
      }
    )
  where
    (unknownFields, conditionals) = unzip (map toConditional $ fromMaybeList commonOptionsWhen)

instance FromJSON (CaptureUnknownFields CustomSetupSection) where
  parseJSON = captureUnknownFields

instance FromJSON (CaptureUnknownFields FlagSection) where
  parseJSON = captureUnknownFields


data Empty = Empty
  deriving (Eq, Show)

instance FromJSON Empty where
  parseJSON _ = return Empty

instance HasFieldNames Empty where
  fieldNames _ = []


data ThenElse = ThenElse {
  _thenElseCondition :: String
, _thenElseThen :: CaptureUnknownFields (Section Empty)
, _thenElseElse :: CaptureUnknownFields (Section Empty)
} deriving (Eq, Show, Generic)

instance FromJSON (CaptureUnknownFields ThenElse) where
  parseJSON = captureUnknownFields

getUnknownFields :: forall a. HasFieldNames a => Value -> Proxy a -> [FieldName]
getUnknownFields v _ = case v of
  Object o -> ignoreUnderscored unknown
    where
      unknown = keys \\ fields
      keys = map T.unpack (HashMap.keys o)
      fields = fieldNames (Proxy :: Proxy a)
      ignoreUnderscored
        | ignoreUnderscoredUnknownFields (Proxy :: Proxy a) = filter (not . isPrefixOf "_")
        | otherwise = id
  _ -> []

captureUnknownFields :: forall a. (HasFieldNames a, FromJSON a) => Value -> Parser (CaptureUnknownFields a)
captureUnknownFields v = CaptureUnknownFields unknown <$> parseJSON v
  where
    unknown = getUnknownFields v (Proxy :: Proxy a)

instance HasFieldNames ThenElse

instance FromJSON ThenElse where
  parseJSON = genericParseJSON_


newtype Condition = Condition {
  conditionCondition :: String
} deriving (Eq, Show, Generic)

instance FromJSON Condition where
  parseJSON = genericParseJSON_

instance HasFieldNames Condition


data CommonOptions = CommonOptions {
  commonOptionsSourceDirs         :: Maybe (List FilePath)
, commonOptionsDependencies       :: Maybe (List Dependency)
, commonOptionsDefaultExtensions  :: Maybe (List String)
, commonOptionsOtherExtensions    :: Maybe (List String)
, commonOptionsGhcOptions         :: Maybe (List GhcOption)
, commonOptionsGhcProfOptions     :: Maybe (List GhcProfOption)
, commonOptionsGhcjsOptions       :: Maybe (List GhcjsOption)
, commonOptionsCppOptions         :: Maybe (List CppOption)
, commonOptionsCcOptions          :: Maybe (List CcOption)
, commonOptionsCSources           :: Maybe (List FilePath)
, commonOptionsJsSources          :: Maybe (List FilePath)
, commonOptionsExtraLibDirs       :: Maybe (List FilePath)
, commonOptionsExtraLibraries     :: Maybe (List FilePath)
, commonOptionsIncludeDirs        :: Maybe (List FilePath)
, commonOptionsInstallIncludes    :: Maybe (List FilePath)
, commonOptionsLdOptions          :: Maybe (List LdOption)
, commonOptionsBuildable          :: Maybe Bool
, commonOptionsWhen               :: Maybe (List ConditionalSection)
, commonOptionsBuildTools         :: Maybe (List Dependency)
} deriving (Eq, Show, Generic)

instance HasFieldNames CommonOptions

instance FromJSON CommonOptions where
  parseJSON = genericParseJSON_


data ConditionalSection = ThenElseConditional (CaptureUnknownFields ThenElse) | FlatConditional (CaptureUnknownFields (Section Condition))
  deriving (Eq, Show)

instance FromJSON ConditionalSection where
  parseJSON v
    | hasKey "then" v || hasKey "else" v = ThenElseConditional <$> parseJSON v
    | otherwise = FlatConditional <$> parseJSON v

hasKey :: Text -> Value -> Bool
hasKey key (Object o) = HashMap.member key o
hasKey _ _ = False
