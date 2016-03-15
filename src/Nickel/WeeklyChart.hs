{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Nickel.WeeklyChart
  ( Week(..)
  , weekOfDay
  , thisWeek
  , mondayOfWeek
  , fstOfYearOnWeek
  , fstOfMonthOnWeek

  , WeeklyData(..)
  , weeklyData
  , weeklyCharts
  , weeklyChart
  ) where

import Nickel.Account
import Nickel.Util

import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import Data.Map (Map)
import Data.Ord
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Control.Lens as Lens
import qualified Data.Colour as Colour
import qualified Data.Colour.Names as Colour
import qualified Data.Default.Class as Default
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Graphics.Rendering.Chart as Chart



data Week = Week Integer Int
  deriving (Eq, Ord, Show, Read)

instance Enum Week where
  toEnum n =
    let (y, w, _) = Time.toWeekDate $ Time.ModifiedJulianDay (7 * fromIntegral n)
    in  Week y w

  fromEnum (Week y w) =
    -- Because the Julian epoch is a Wednesday.
    let day = Time.fromWeekDate y w 3
    in  fromIntegral (Time.toModifiedJulianDay day `div` 7)

instance Chart.PlotValue Week where
  toValue (Week y w) =
    let day = Time.fromWeekDate y w 1
    in  fromIntegral $ Time.toModifiedJulianDay day

  fromValue x =
    let (y, w, _) = Time.toWeekDate . Time.ModifiedJulianDay $ floor x
    in  Week y w

  autoAxis (Set.toList . Set.fromList -> weeks) = Chart.AxisData
    { Chart._axis_visibility = Default.def
    , Chart._axis_viewport = Chart.vmap (mi, ma)
    , Chart._axis_tropweiv = Chart.invmap (mi, ma)
    , Chart._axis_ticks = map (,2) weeks
    , Chart._axis_grid = []
    , Chart._axis_labels = map catMaybes . transpose $ map label weeks
    }
    where
      mi = pred $ minimum weeks
      ma = succ $ maximum weeks
      label yw@(weekLabel -> (yearLab, monthLab)) =
        [(yw, ) <$> monthLab, (yw, ) <$> yearLab]

weekOfDay :: Time.Day -> Week
weekOfDay (Time.toWeekDate -> (y, w, _)) = Week y w

thisWeek :: IO Week
thisWeek =
  weekOfDay . Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime

mondayOfWeek :: Week -> (Integer, Int, Int)
mondayOfWeek (Week y w) = (y', m, d)
  where
    (y', m, d) = Time.toGregorian $ Time.fromWeekDate y w 1

fstOfYearOnWeek :: Week -> Maybe Time.Day
fstOfYearOnWeek yw@(Week y _)
  | yw == weekOfDay thisJan1 = Just thisJan1
  | yw == weekOfDay nextJan1 = Just nextJan1
  | otherwise = Nothing
  where
    thisJan1 = Time.fromGregorian y 1 1
    nextJan1 = Time.fromGregorian (y + 1) 1 1

fstOfMonthOnWeek :: Week -> Maybe Time.Day
fstOfMonthOnWeek yw
  | yw == weekOfDay thisMo1 = Just thisMo1
  | yw == weekOfDay nextMo1 = Just nextMo1
  | otherwise = Nothing
  where
    thisMo1 = Time.fromGregorian y m 1
    nextMo1
      | m == 12 = Time.fromGregorian (y + 1) 1 1
      | otherwise = Time.fromGregorian y (m + 1) 1
    (y, m, _) = mondayOfWeek yw



data WeeklyData = WeeklyData
  { wdSums :: Map Week Int
  , wdSmoothSums :: Map Week Int
  , wdName :: String
  , wdRelevancy :: Int
  } deriving (Eq, Show)

weeklyData :: Week -> String -> [Expense] -> WeeklyData
weeklyData currentWeek name exps = WeeklyData{..}
  where
    wdSums = fillMissingInnerWeeks sums
    wdSmoothSums = smoothSums currentWeek sums
    wdName = name
    wdRelevancy = length exps
    sums = fmap (sum . map expAmount) . tagGroupBy (weekOfDay . expDate) $ exps

weeklyCharts :: [WeeklyData] -> Chart.StackedLayouts Week
weeklyCharts wds = Default.def
  & Lens.set Chart.slayouts_layouts layouts
  where
    layouts = map Chart.StackedLayout charts
    charts = map weeklyChart . sortBy (comparing $ Down . wdRelevancy) $ wds

weeklyChart :: WeeklyData -> Chart.Layout Week Int
weeklyChart WeeklyData{..} = Default.def
  & Lens.set Chart.layout_title wdName
  . Lens.set Chart.layout_plots [sumPlot, smoothSumPlot]
  where
    sumPlot = Chart.plotBars $ Default.def
      & Lens.set Chart.plot_bars_values sumValues
      & Lens.set Chart.plot_bars_item_styles [(fillStyle, Nothing)]
    sumValues = Map.toList . fmap pure $ wdSums
    fillStyle = Chart.FillStyleSolid $ Colour.opaque Colour.lightsteelblue
    smoothSumPlot = Chart.toPlot $ Default.def
      & Lens.set Chart.plot_lines_values smoothSumValues
      & Lens.set Chart.plot_lines_style lineStyle
    smoothSumValues = [Map.toList wdSmoothSums]
    lineStyle = Default.def
      & Lens.set Chart.line_color (Colour.opaque Colour.midnightblue)
      & Lens.set Chart.line_width 1.5



fillMissingInnerWeeks :: Num a => Map Week a -> Map Week a
fillMissingInnerWeeks weekSums = Map.union weekSums zeroWeeks
  where
    zeroWeeks = case minMax' $ Map.keysSet weekSums of
      Nothing -> Map.empty
      Just (mi, ma) -> zeroes mi ma

smoothSums :: Week -> Map Week Int -> Map Week Int
smoothSums currentWeek weeksSums = Map.fromAscList $ zip closedWeeks avgs
  where
    closedWeeks = takeWhile (< currentWeek) weeks
    avgs = map (floor :: Double -> Int) . movingAvgs 2 . map fromIntegral $ sums
    (weeks, sums) = unzip . Map.toList $ Map.union weeksSums zeroWeeks
    zeroWeeks = case minimum' $ Map.keysSet weeksSums of
      Nothing -> Map.empty
      Just mi -> zeroes mi currentWeek

zeroes :: Num a => Week -> Week -> Map Week a
zeroes mi ma = Map.fromList . map (,0) $ enumFromTo mi ma

-- Show the next year/month in the label if the year/month changes that week.
weekLabel :: Week -> (Maybe String, Maybe String)
weekLabel = yearMonthLabel . fstYearMonth
  where
    yearMonthLabel = fmap yearLabel *** fmap monthLabel
    fstYearMonth = fstOfYearOnWeek &&& fstOfMonthOnWeek
    yearLabel (Time.toGregorian -> (y, _, _)) = show y
    monthLabel (Time.toGregorian -> (_, m, _)) = monthNames !! (m - 1)

monthNames :: [String]
monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
