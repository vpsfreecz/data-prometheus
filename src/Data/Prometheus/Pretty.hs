{-# LANGUAGE OverloadedStrings #-}
module Data.Prometheus.Pretty where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Data.Prometheus.Types

prettyMetrics :: PromMetrics -> B.ByteString
prettyMetrics = B.concat . M.elems . M.mapWithKey prettyMetric

prettyMetric :: MetricId -> Metric -> B.ByteString
prettyMetric mId mData = B.unlines [
    prettyHelp mId
  , prettyType mId mData
  , B.unwords [ prettyId mId , prettyData mData ]
  ]

prettyMetricShort :: MetricId -> Metric -> B.ByteString
prettyMetricShort mId mData = B.unwords [ prettyId mId , prettyData mData ]

prettyHelp :: MetricId -> B.ByteString
prettyHelp (MetricId name help _) = B.unwords [ "# HELP", name, help ]

prettyType :: MetricId -> Metric -> B.ByteString
prettyType mId x = B.unwords [ "# TYPE", name mId, toTypeStr x ]

toTypeStr (Counter _)   = "counter"
toTypeStr (Gauge _)     = "gauge"
toTypeStr (Summary _ _ _)   = "summary"
toTypeStr (Histogram _ _ _) = "histogram"

prettyData (Counter c) = B.pack $ show c
prettyData (Gauge g) = B.pack $ show g
prettyData _ = error "Unable to print summary or histogram"

prettyId :: MetricId -> B.ByteString
prettyId (MetricId name help labels) = B.concat [ name, prettyLabels labels ]

prettyLabels :: Labels -> B.ByteString
prettyLabels labels | M.null labels = ""
prettyLabels labels                 = B.concat
  [
    "{"
  , B.intercalate "," $ M.elems $ M.mapWithKey (\k v -> B.concat [k, "=\"", v, "\""]) labels
  , "}"
  ]
