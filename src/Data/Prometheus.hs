module Data.Prometheus
  ( parseProm
  , filterMetrics
  , findMetrics
  , hasLabel
  , byLabel
  , byLabel'
  , module T
  ) where

import Data.Prometheus.Monad  as T
import Data.Prometheus.Parse  as T
import Data.Prometheus.Pretty as T
import Data.Prometheus.Types  as T
import Data.Attoparsec.ByteString.Char8

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

-- | Parse Prometheus metrics from ByteString
parseProm :: B.ByteString -> Either String PromMetrics
parseProm = parseOnly parseMetrics

-- | Filter metrics where name is prefixed by `pattern`
filterMetrics :: String -> M.Map MetricId a -> M.Map MetricId a
filterMetrics pattern = M.filterWithKey (\k _ -> (B.pack pattern) `B.isPrefixOf` (name k))

-- | Find metrics where name is equal to `pattern`
findMetrics :: String -> M.Map MetricId a -> M.Map MetricId a
findMetrics pattern = M.filterWithKey (\k _ -> (B.pack pattern) == (name k))

-- | Find metrics by `label`
hasLabel :: String -> M.Map MetricId a -> M.Map MetricId a
hasLabel label' = M.filterWithKey (\k _ -> (M.member (B.pack label') (labels k)))

-- | Find metrics with `label` which matches `contents`
byLabel :: String -> B.ByteString -> M.Map MetricId a -> M.Map MetricId a
byLabel label' contents = M.filterWithKey (\k _ -> (
  case M.lookup (B.pack label') (labels k) of
    Nothing -> False
    Just lc -> lc == contents
  ))

-- | Find metrics with `label` which content satisfies `op` predicate
byLabel' :: String -> (B.ByteString -> Bool) -> M.Map MetricId a -> M.Map MetricId a
byLabel' label' op = M.filterWithKey (\k _ -> (
  case M.lookup (B.pack label') (labels k) of
    Nothing -> False
    Just lc -> op lc
  ))

-- byLabels
