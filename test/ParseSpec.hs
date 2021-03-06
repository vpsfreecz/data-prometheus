{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes #-}

module ParseSpec where

import SpecHelper
import Text.RawString.QQ
import Data.Attoparsec.ByteString.Char8
import qualified Data.Map as M

gauge = [r|# HELP go_memstats_alloc_bytes Number of bytes allocated and still in use.
# TYPE go_memstats_alloc_bytes gauge
go_memstats_alloc_bytes 1.276272e+06
|]

summary = [r|# HELP go_gc_duration_seconds A summary of the GC invocation durations.
# TYPE go_gc_duration_seconds summary
go_gc_duration_seconds{quantile="0"} 8.1545e-05
go_gc_duration_seconds{quantile="0.25"} 0.000103978
go_gc_duration_seconds{quantile="0.5"} 0.000118208
go_gc_duration_seconds{quantile="0.75"} 0.000140686
go_gc_duration_seconds{quantile="1"} 0.000302995
go_gc_duration_seconds_sum 0.006560726
go_gc_duration_seconds_count 51
|]

counter = [r|# HELP node_context_switches_total Total number of context switches.
# TYPE node_context_switches_total counter
node_context_switches_total 1.54296968e+08
|]

countersLabels = [r|# HELP node_cpu_core_throttles_total helptext
# TYPE node_cpu_core_throttles_total counter
node_cpu_core_throttles_total{core="0",package="0"} 0
node_cpu_core_throttles_total{core="1",package="0"} 0
|]

multiple = [r|# HELP node_arp_entries ARP entries by device
# TYPE node_arp_entries gauge
node_arp_entries{device="eth0"} 2
# HELP node_boot_time_seconds Node boot time, in unixtime.
# TYPE node_boot_time_seconds gauge
node_boot_time_seconds 1.537903224e+09
# HELP node_context_switches_total Total number of context switches.
# TYPE node_context_switches_total counter
node_context_switches_total 1.54296968e+08
|]

testCases = [
   ("", Left "not enough input")
 , (gauge, Right $ M.fromList [(MetricId {name = "go_memstats_alloc_bytes", help = "Number of bytes allocated and still in use.", labels = M.empty}
                  ,Gauge 1276272.0)])

 , (summary, Right  $ M.fromList [(MetricId {name = "go_gc_duration_seconds", help = "A summary of the GC invocation durations.", labels = M.empty},
                     Summary {sumQuantiles = M.fromList [(0.0,8.1545e-5),(0.25,1.03978e-4),(0.5,1.18208e-4),(0.75,1.40686e-4),(1.0,3.02995e-4)], sumSum = 6.560726e-3, sumCount = 51.0})])

 , (counter, Right  $ M.fromList [(MetricId {name = "node_context_switches_total", help = "Total number of context switches.", labels = M.empty}
                    ,Counter 1.54296968e8)])

 , (countersLabels, Right $ M.fromList
      [(MetricId {name = "node_cpu_core_throttles_total", help = "helptext", labels = M.fromList [("core","0"),("package","0")]}
                 , Counter 0.0)
      ,(MetricId {name = "node_cpu_core_throttles_total", help = "helptext", labels = M.fromList [("core","1"),("package","0")]}
               ,Counter 0.0)])
 , (multiple, Right  $ M.fromList
      [(MetricId {name = "node_arp_entries", help = "ARP entries by device", labels = M.fromList [("device","eth0")]}
       , Gauge 2.0)
      ,(MetricId {name = "node_boot_time_seconds", help = "Node boot time, in unixtime.", labels = M.fromList []}
       , Gauge 1.537903224e9)
      ,(MetricId {name = "node_context_switches_total", help = "Total number of context switches.", labels = M.fromList []}
       , Counter 1.54296968e8)])
 ]

spec :: Spec
spec = do
  it "parses samples" $ do
    mapM_ (\(x, y) -> parseOnly parseMetrics x `shouldBe` y) testCases

main :: IO ()
main = do
  hspec spec
