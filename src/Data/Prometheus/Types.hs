module Data.Prometheus.Types where

import Data.Map (Map)
import qualified Data.ByteString.Char8 as B

type Labels = Map B.ByteString B.ByteString
type PromMetrics = Map MetricId Metric

data MetricId = MetricId
  { name :: B.ByteString
  , help :: B.ByteString
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
    , histSum :: Double
    , histCount :: Double
    }
  deriving (Eq, Ord, Show)
