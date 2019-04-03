data-prometheus
===============

Prometheus metrics parser, pretty printer and filtering utilities for querying `node_exporter` style
exporters.

Usage
-----

```haskell
import Data.Prometheus

import Network.Wreq
import Data.Prometheus

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  r <- get "http://localhost:9100/metrics"
  case parseProm (BL.toStrict $ r ^. responseBody) of
    Right result -> print result
    Left err -> putStrLn err
```
