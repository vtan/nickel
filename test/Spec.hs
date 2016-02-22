module Main (main) where

import qualified Gold.MainSpec
import qualified Gold.AccountSpec
import qualified Gold.UtilSpec
import qualified Gold.WeeklyChartSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Gold.Main" Gold.MainSpec.spec
  describe "Gold.Account" Gold.AccountSpec.spec
  describe "Gold.Util" Gold.UtilSpec.spec
  describe "Gold.WeeklyChart" Gold.WeeklyChartSpec.spec
