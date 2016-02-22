{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Gold.Main where

import Gold.Account
import Gold.Util
import Gold.WeeklyChart

import Control.Monad
import System.IO
import qualified Data.Map as Map

import qualified Data.Time as Time
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart



main :: IO ()
main = do
  content <- readFile "/home/vtan/doc/pez"
  today <- Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime
  let
    w = weekOfDay today
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
