module Hpack.Config.PackageConfig where

import           Data.Aeson.Types
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Text (Text)
import           Hpack.Config.BuildType
import           Hpack.Config.CommonOptions
import           Hpack.Config.ExecutableSection
import           Hpack.Config.FlagSection
import           Hpack.Config.GenericParseJson
import           Hpack.Config.HasFieldNames
import           Hpack.Util

data PackageConfig = PackageConfig {
  packageConfigName             :: Maybe String
, packageConfigVersion          :: Maybe String
, packageConfigSynopsis         :: Maybe String
, packageConfigDescription      :: Maybe String
, packageConfigHomepage         :: Maybe (Maybe String)
, packageConfigBugReports       :: Maybe (Maybe String)
, packageConfigCategory         :: Maybe String
, packageConfigStability        :: Maybe String
, packageConfigAuthor           :: Maybe (List String)
, packageConfigMaintainer       :: Maybe (List String)
, packageConfigCopyright        :: Maybe (List String)
, packageConfigBuildType        :: Maybe BuildType
, packageConfigLicense          :: Maybe String
, packageConfigLicenseFile      :: Maybe (List String)
, packageConfigTestedWith       :: Maybe String
, packageConfigFlags            :: Maybe (Map String (CaptureUnknownFields FlagSection))
, packageConfigExtraSourceFiles :: Maybe (List FilePath)
, packageConfigDataFiles        :: Maybe (List FilePath)
, packageConfigGithub           :: Maybe Text
, packageConfigGit              :: Maybe String
, packageConfigCustomSetup      :: Maybe (CaptureUnknownFields CustomSetupSection)
, packageConfigLibrary          :: Maybe (CaptureUnknownFields (Section LibrarySection))
, packageConfigExecutables      :: Maybe (Map String (CaptureUnknownFields (Section ExecutableSection)))
, packageConfigTests            :: Maybe (Map String (CaptureUnknownFields (Section ExecutableSection)))
, packageConfigBenchmarks       :: Maybe (Map String (CaptureUnknownFields (Section ExecutableSection)))
} deriving (Eq, Show, Generic)

instance HasFieldNames PackageConfig where
  ignoreUnderscoredUnknownFields _ = True

instance FromJSON PackageConfig where
  parseJSON value = handleNullValues <$> genericParseJSON_ value
    where
      handleNullValues :: PackageConfig -> PackageConfig
      handleNullValues =
          ifNull "homepage" (\p -> p {packageConfigHomepage = Just Nothing})
        . ifNull "bug-reports" (\p -> p {packageConfigBugReports = Just Nothing})

      ifNull :: String -> (a -> a) -> a -> a
      ifNull name f
        | isNull name value = f
        | otherwise = id

isNull :: String -> Value -> Bool
isNull name value = case parseMaybe p value of
  Just Null -> True
  _ -> False
  where
    p = parseJSON >=> (.: fromString name)
