{-# LANGUAGE DeriveGeneric #-}

module Hpack.Config.FlagSection where

import Data.Aeson.Types
import GHC.Generics
import Hpack.Config.GenericParseJson
import Hpack.Config.HasFieldNames

data FlagSection = FlagSection {
  _flagSectionDescription :: Maybe String
, _flagSectionManual :: Bool
, _flagSectionDefault :: Bool
} deriving (Eq, Show, Generic)

instance HasFieldNames FlagSection

instance FromJSON FlagSection where
  parseJSON = genericParseJSON_
