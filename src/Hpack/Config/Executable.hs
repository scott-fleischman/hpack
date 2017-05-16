module Hpack.Config.Executable where

data Executable = Executable {
  executableName :: String
, executableMain :: FilePath
, executableOtherModules :: [String]
} deriving (Eq, Show)
