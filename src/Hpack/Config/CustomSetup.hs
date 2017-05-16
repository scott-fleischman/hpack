module Hpack.Config.CustomSetup where

import Hpack.Config.Dependency

data CustomSetup = CustomSetup {
  customSetupDependencies :: [Dependency]
} deriving (Eq, Show)
