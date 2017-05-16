{-# LANGUAGE DeriveGeneric #-}

module Hpack.Config.CustomSetupSection where

import Data.Aeson.Types
import GHC.Generics
import Hpack.Config.Dependency
import Hpack.Config.GenericParseJson
import Hpack.Config.HasFieldNames
import Hpack.Util

data CustomSetupSection = CustomSetupSection {
  customSetupSectionDependencies :: Maybe (List Dependency)
} deriving (Eq, Show, Generic)

instance HasFieldNames CustomSetupSection

instance FromJSON CustomSetupSection where
  parseJSON = genericParseJSON_
