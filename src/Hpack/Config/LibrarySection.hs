{-# LANGUAGE DeriveGeneric #-}

module Hpack.Config.LibrarySection where

import Data.Aeson.Types
import GHC.Generics
import Hpack.Config.GenericParseJson
import Hpack.Config.HasFieldNames
import Hpack.Util

data LibrarySection = LibrarySection {
  librarySectionExposed           :: Maybe Bool
, librarySectionExposedModules    :: Maybe (List String)
, librarySectionOtherModules      :: Maybe (List String)
, librarySectionReexportedModules :: Maybe (List String)
} deriving (Eq, Show, Generic)

instance HasFieldNames LibrarySection

instance FromJSON LibrarySection where
  parseJSON = genericParseJSON_
