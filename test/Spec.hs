module Main (main) where

import qualified Nickel.MainSpec
import qualified Nickel.AccountSpec
import qualified Nickel.UtilSpec
import qualified Nickel.WeeklyChartSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Nickel.Main" Nickel.MainSpec.spec
  describe "Nickel.Account" Nickel.AccountSpec.spec
  describe "Nickel.Util" Nickel.UtilSpec.spec
  describe "Nickel.WeeklyChart" Nickel.WeeklyChartSpec.spec
