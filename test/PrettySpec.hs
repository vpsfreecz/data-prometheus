{-# LANGUAGE OverloadedStrings #-}

module PrettySpec where

import SpecHelper
import qualified Data.Map as M

sIdSimple = MetricId "name" "help" M.empty
sIdLabels = MetricId "name" "help" (M.fromList [("a", "b")])
sC  = Counter 666
sG  = Gauge 123.456

spec :: Spec
spec = do
  it "pretty prints ID with labels" $ do
    prettyId sIdLabels `shouldBe` "name{a=\"b\"}"

  it "pretty prints simple ID" $ do
    prettyId sIdSimple `shouldBe` "name"

  it "pretty prints conter" $ do
    prettyData sC `shouldBe` "666.0"

  it "pretty prints gauge" $ do
    prettyData sG `shouldBe` "123.456"

  it "pretty prints full metric with help and type" $ do
    prettyMetric sIdLabels sC `shouldBe` "# HELP name help\n# TYPE name counter\nname{a=\"b\"} 666.0\n"

  it "pretty prints metric" $ do
    prettyMetricShort sIdLabels sC `shouldBe` "name{a=\"b\"} 666.0"

main :: IO ()
main = do
  hspec spec
