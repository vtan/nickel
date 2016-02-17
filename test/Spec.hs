module Main (main) where

import qualified Gold.MainSpec
import qualified Gold.ExpenseSpec
import qualified Gold.UtilSpec
import qualified Gold.WeeklyChartSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  Gold.MainSpec.spec
  Gold.ExpenseSpec.spec
  Gold.UtilSpec.spec
  Gold.WeeklyChartSpec.spec
