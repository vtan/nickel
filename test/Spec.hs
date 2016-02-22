module Main (main) where

import qualified Gold.MainSpec
import qualified Gold.AccountSpec
import qualified Gold.UtilSpec
import qualified Gold.WeeklyChartSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  Gold.MainSpec.spec
  Gold.AccountSpec.spec
  Gold.UtilSpec.spec
  Gold.WeeklyChartSpec.spec
