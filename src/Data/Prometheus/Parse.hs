{-# LANGUAGE OverloadedStrings #-}
module Data.Prometheus.Parse where

import Control.Applicative

import Prelude hiding (takeWhile)
import qualified Data.Map as M
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8
import Data.Prometheus.Types

parseMetrics :: Parser PromMetrics
parseMetrics = M.fromList . concat <$> many1 parseMetric <* endOfInput

--parseMetric :: Parser PromMetrics
parseMetric = do
  (name, help, typ) <- parseMeta
  lm <- case typ of
    "counter" -> parseCounters
    "gauge" -> parseGauges
    "untyped" -> parseGauges -- untyped /o\
    "histogram" -> parseHistogram
    "summary" -> parseSummary
    x -> fail $ show x

  return $ map (\(labels, metric) -> (MetricId name help labels, metric)) lm

-- name, help, textual type
parseMeta :: Parser (ByteString, ByteString, ByteString)
parseMeta = do
  "# HELP "
  name <- word
  space
  help <- eol
  endOfLine
  "# TYPE "
  word
  space
  typ <- word
  endOfLine
  return (name, help, typ)
  where
    eol :: Parser ByteString
    eol = takeWhile (/= '\n')

    word :: Parser ByteString
    word  = takeWhile1 (\x -> x /=' ' && x /= '\n')

parseGauges :: Parser [(Labels, Metric)]
parseGauges = many1 (labelsValue (Gauge <$> double))

parseCounters :: Parser [(Labels, Metric)]
parseCounters = many1 (labelsValue (Counter <$> double))

parseSummary :: Parser [(Labels, Metric)]
parseSummary = do
  qs <- M.fromList <$> parseQuantiles `sepBy` endOfLine <?> "quantiles"
  (_, sum) <- labelsValue double
  (_, cnt) <- labelsValue double
  return $ [(M.empty, Summary qs sum cnt)]

parseQuantiles :: Parser (Double, Double)
parseQuantiles = do
  takeWhile1 (\x -> x /= '{' && x /= ' ')
  q <- "{quantile=\"" *> double <* "\"}"
  space
  val <- double
  return (q, val)

parseHistogram :: Parser [(Labels, Metric)]
parseHistogram = do
  qs <- M.fromList <$> parseHistBuckets `sepBy` endOfLine <?> "quantiles"
  (_, sum) <- labelsValue double
  (_, cnt) <- labelsValue double
  return $ [(M.empty, Histogram qs sum cnt)]

parseHistBuckets :: Parser (Double, Double)
parseHistBuckets = do
  takeWhile1 (\x -> x /= '{' && x /= ' ')
  q <- "{le=\"" *> double <* "\"}"
  space
  val <- double
  return (q, val)

labelsValue :: Parser b -> Parser (Labels, b)
labelsValue f = do
  takeWhile1 (\x -> x /= '{' && x /= ' ')
  ls <- option M.empty (char '{' *> parseLabels <* char '}')
  space
  val <- f
  endOfLine
  return (ls, val)

parseLabels :: Parser Labels
parseLabels = M.fromList <$> parseLabel `sepBy1` (char ',')

parseLabel :: Parser (ByteString, ByteString)
parseLabel = do
  l <- takeWhile (/= '=')
  char '='
  v <- char '"' *> takeWhile (\x -> x /= '"') <* char '"'
  return (l, v)
