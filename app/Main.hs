module Main where

import Data.Prometheus
import Data.Prometheus.Parse
import Data.Attoparsec.ByteString.Char8

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Control.Lens
import Network.Wreq

import qualified Codec.Compression.GZip as GZip

main = do
  r <- get "http://localhost:9100/metrics"
  case parseOnly parseMetrics (BL.toStrict $ r ^. responseBody) of
    --Right result -> BL.putStr $ GZip.compress $ BL.fromStrict $ B.pack $ show result
    Right result -> print result
    Left err -> putStrLn err
