module Data.Prometheus
  ( parseProm
  , filterMetrics
  , findMetrics
  , hasLabel
  , byLabel
  , byLabel'
  , module T
  ) where

import Data.Prometheus.Parse as T
import Data.Prometheus.Pretty as T
import Data.Prometheus.Types as T
import Data.Attoparsec.ByteString.Char8

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

parseProm = parseOnly parseMetrics

filterMetrics pattern = M.filterWithKey (\k _ -> (B.pack pattern) `B.isPrefixOf` (name k))

findMetrics pattern = M.filterWithKey (\k _ -> (B.pack pattern) == (name k))

hasLabel label = M.filterWithKey (\k _ -> (M.member (B.pack label) (labels k)))

byLabel label contents = M.filterWithKey (\k _ -> (
  case M.lookup (B.pack label) (labels k) of
    Nothing -> False
    Just lc -> lc == contents
  ))

byLabel' label op = M.filterWithKey (\k _ -> (
  case M.lookup (B.pack label) (labels k) of
    Nothing -> False
    Just lc -> op lc
  ))

-- byLabels
