module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, module Control.Applicative
, withTempDirectory
, module System.FilePath
, withCurrentDirectory
, Empty(..)
) where

import           Test.Hspec
import           Test.Mockery.Directory
import           Control.Applicative
import           System.Directory (getCurrentDirectory, setCurrentDirectory, canonicalizePath)
import           Control.Exception
import qualified System.IO.Temp as Temp
import           System.FilePath
import           Data.Aeson (FromJSON(..))
import           Hpack.Config (HasFieldNames(..))

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory action = Temp.withSystemTempDirectory "hspec" $ \dir -> do
  canonicalizePath dir >>= action

data Empty = Empty
  deriving (Eq, Show)

instance FromJSON Empty where
  parseJSON _ = return Empty

instance HasFieldNames Empty where
  fieldNames _ = []
