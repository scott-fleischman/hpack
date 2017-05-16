module Hpack.Config.AddSource where

import Hpack.Config.GitHub

data AddSource = GitRef GitUrl GitRef (Maybe FilePath) | Local FilePath
  deriving (Eq, Show, Ord)
