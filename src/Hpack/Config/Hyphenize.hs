{-# LANGUAGE CPP #-}

module Hpack.Config.Hyphenize where

import Data.Aeson.Types

hyphenize :: String -> String -> String
hyphenize name =
#if MIN_VERSION_aeson(0,10,0)
  camelTo2
#else
  camelTo
#endif
  '-' . drop (length name) . dropWhile (== '_')
