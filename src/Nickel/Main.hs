module Nickel.Main (main) where

import Nickel.Account
import Nickel.Util
import Nickel.WeeklyChart

import Control.Arrow
import Control.Monad
import Data.List
import Data.Ord
import System.IO

import qualified Data.Map as Map
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart



main :: IO ()
main = do
  content <- getContents
  w <- thisWeek
  let
    accs = parseAccounts . lines $ content
    exps = [e | ParsedExpense e <- accs]
    catsWds =
      map (id *** weeklyData w)
      . sortBy (comparing $ Down . length . snd)
      . Map.toList
      . tagGroupBy expCat
      $ exps
    totalWd = ("Total", weeklyData w exps)
    chart =
      flip Chart.StackedLayouts False
      . map (Chart.StackedLayout . uncurry weeklyChart)
      $ totalWd : catsWds
    heightFactor = length catsWds
    format = Chart.FileOptions (1000, heightFactor * 500) Chart.SVG
  void . Chart.renderableToFile format "weekly.svg" . Chart.toRenderable $ chart
  forM_ [n | InvalidLine n <- accs] $ \n ->
    hPutStrLn stderr $ "Invalid line " ++ show n
