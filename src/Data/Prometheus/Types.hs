module Data.Prometheus.Types where

import Data.Map (Map)
import Data.ByteString (ByteString)

type Labels = Map ByteString ByteString
type PromMetrics = Map MetricId Metric

data MetricId = MetricId
  { name   :: ByteString
  , help   :: ByteString
  , labels :: Labels }
  deriving (Eq, Ord, Show)

data Metric
  = Counter Double
  | Gauge Double
  | Summary
    { sumQuantiles :: Map Double Double
    , sumSum       :: Double
    , sumCount     :: Double
    }
  | Histogram
    { histBuckets :: Map Double Double
    , histSum     :: Double
    , histCount   :: Double
    }
  deriving (Eq, Ord, Show)
