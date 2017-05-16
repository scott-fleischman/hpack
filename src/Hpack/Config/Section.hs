{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Hpack.Config.Section where

import Hpack.Config.BuildType
import Hpack.Config.CustomSetup
import Hpack.Config.Dependency
import Hpack.Config.Executable
import Hpack.Config.Flag
import Hpack.Config.Library
import Hpack.Config.SourceRepository
import Hpack.Util

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
