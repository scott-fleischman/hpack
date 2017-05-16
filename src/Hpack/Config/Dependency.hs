{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hpack.Config.Dependency where

import Control.Applicative
import Data.Aeson.Types
import Data.String
import GHC.Generics
import Hpack.Config.AddSource
import Hpack.Config.GitHub

data Dependency = Dependency {
  dependencyName :: String
, dependencyGitRef :: Maybe AddSource
} deriving (Eq, Show, Ord, Generic)

instance IsString Dependency where
  fromString name = Dependency name Nothing

instance FromJSON Dependency where
  parseJSON v = case v of
    String _ -> fromString <$> parseJSON v
    Object o -> addSourceDependency o
    _ -> typeMismatch "String or an Object" v
    where
      addSourceDependency o = Dependency <$> name <*> (Just <$> (local <|> git))
        where
          name :: Parser String
          name = o .: "name"

          local :: Parser AddSource
          local = Local <$> o .: "path"

          git :: Parser AddSource
          git = GitRef <$> url <*> ref <*> subdir

          url :: Parser String
          url =
                ((githubBaseUrl ++) <$> o .: "github")
            <|> (o .: "git")
            <|> fail "neither key \"git\" nor key \"github\" present"

          ref :: Parser String
          ref = o .: "ref"

          subdir :: Parser (Maybe FilePath)
          subdir = o .:? "subdir"
