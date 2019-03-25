{-# LANGUAGE OverloadedStrings #-}
module Data.Prometheus.Pretty where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Data.Prometheus.Types

prettyID (MetricId name help labels) = B.concat [ name, prettyLabels labels ]

prettyLabels labels | M.null labels = ""
prettyLabels labels                 = B.concat
  [
    "{"
  , B.intercalate "," $ M.elems $ M.mapWithKey (\k v -> B.concat [k, "=\"", v, "\""]) labels
  , "}"
  ]
