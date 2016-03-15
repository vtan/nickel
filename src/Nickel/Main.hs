module Nickel.Main (main) where

import Nickel.Account
import Nickel.Util
import Nickel.WeeklyChart

import Control.Monad
import System.IO
import qualified Data.Map as Map

import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart



main :: IO ()
main = do
  content <- readFile "/home/vtan/doc/pez"
  w <- thisWeek
  let
    accs = parseAccounts . lines $ content
    exps = [e | ParsedExpense e <- accs]
    catsExpss = tagGroupBy expCat $ exps
    catsWds = Map.mapWithKey (\cat -> weeklyData w cat) catsExpss
    chart = weeklyCharts . Map.elems $ catsWds
    heightFactor = Map.size catsWds
    format = Chart.FileOptions (1000, heightFactor * 500) Chart.SVG
  void . Chart.renderableToFile format "weekly.svg" . Chart.toRenderable $ chart
  forM_ [n | InvalidLine n <- accs] $ \n ->
    hPutStrLn stderr $ "Invalid line " ++ show n
