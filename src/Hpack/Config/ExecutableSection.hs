{-# LANGUAGE DeriveGeneric #-}

module Hpack.Config.ExecutableSection where

import Data.Aeson.Types
import GHC.Generics
import Hpack.Config.GenericParseJson
import Hpack.Config.HasFieldNames
import Hpack.Util

data ExecutableSection = ExecutableSection {
  executableSectionMain :: FilePath
, executableSectionOtherModules :: Maybe (List String)
} deriving (Eq, Show, Generic)

instance HasFieldNames ExecutableSection

instance FromJSON ExecutableSection where
  parseJSON = genericParseJSON_

