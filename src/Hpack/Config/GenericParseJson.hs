{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hpack.Config.GenericParseJson where

import Data.Aeson.Types
import Data.Proxy
import GHC.Generics
import Hpack.Config.Hyphenize
import Hpack.GenericsUtil

#if MIN_VERSION_aeson(1,0,0)
genericParseJSON_ :: forall a. (Generic a, GFromJSON Zero (Rep a), HasTypeName a) => Value -> Parser a
#else
genericParseJSON_ :: forall a. (Generic a, GFromJSON (Rep a), HasTypeName a) => Value -> Parser a
#endif
genericParseJSON_ = genericParseJSON defaultOptions {fieldLabelModifier = hyphenize name}
  where
    name :: String
    name = typeName (Proxy :: Proxy a)
