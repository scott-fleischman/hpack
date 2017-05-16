{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Hpack.Config.HasFieldNames where

import Data.Proxy
import GHC.Generics
import Hpack.Config.Hyphenize
import Hpack.GenericsUtil

type FieldName = String

class HasFieldNames a where
  fieldNames :: Proxy a -> [FieldName]

  default fieldNames :: (HasTypeName a, Selectors (Rep a)) => Proxy a -> [String]
  fieldNames proxy = map (hyphenize $ typeName proxy) (selectors proxy)

  ignoreUnderscoredUnknownFields :: Proxy a -> Bool
  ignoreUnderscoredUnknownFields _ = False
