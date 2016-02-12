{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Gold.WeeklyChart where

import Gold.Expense
import Gold.Util

import Control.Monad
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



data Week = Week Int Int
  deriving (Eq, Ord, Show, Read)

instance Enum Week where
  toEnum n =
    let (y, w, _) = Time.toWeekDate $ Time.ModifiedJulianDay (7 * fromIntegral n)
    in  Week (fromIntegral y) w

  fromEnum (Week y w) =
    -- Because the Julian epoch is a Wednesday.
    let day = Time.fromWeekDate (fromIntegral y) w 3
    in  fromIntegral (Time.toModifiedJulianDay day `div` 7)

instance Chart.PlotValue Week where
  toValue (Week y w) =
    let day = Time.fromWeekDate (fromIntegral y) w 1
    in  fromIntegral $ Time.toModifiedJulianDay day

  fromValue x =
    let (y, w, _) = Time.toWeekDate . Time.ModifiedJulianDay $ floor x
    in  Week (fromIntegral y) w

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
weekOfDay (Time.toWeekDate -> (y, w, _)) = Week (fromIntegral y) w



data WeeklyData = WeeklyData
  { wdSums :: Map Week Int
  , wdSmoothSums :: Map Week Int
  , wdName :: String
  , wdRelevancy :: Int
  } deriving (Eq, Show)

weeklyData :: Week -> String -> [Expense] -> WeeklyData
weeklyData currentWeek name exps = WeeklyData{..}
  where
    wdSums = fillMissingInnerWeeks 0
           . fmap (sum . map expAmount)
           . tagGroupBy (weekOfDay . expDate)
           $ exps
    wdSmoothSums = smoothSums currentWeek wdSums
    wdName = name
    wdRelevancy = length exps

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



fillMissingInnerWeeks :: a -> Map Week a -> Map Week a
fillMissingInnerWeeks value weekSums = weekSums `Map.union` zeroes
  where
    zeroes
      | Map.null weekSums = Map.empty
      | otherwise = Map.fromList . map (, value) $ enumFromTo mi ma
    (mi, _) = Map.findMin weekSums
    (ma, _) = Map.findMax weekSums

smoothSums :: Week -> Map Week Int -> Map Week Int
smoothSums currentWeek weeksSums = Map.fromAscList $ zip closedWeeks avgs
  where
    closedWeeks
      | last weeks == currentWeek = init weeks
      | otherwise = weeks
    avgs = map (floor :: Double -> Int) . movingAvgs 2 . map fromIntegral $ sums
    (weeks, sums) = unzip . Map.toList $ weeksSums

-- Show the next year/month in the label if the year/month changes that week.
weekLabel :: Week -> (Maybe String, Maybe String)
weekLabel (Week year week) = (yearLab, monthLab)
  where
    yearLab = show sundayYear <$ guard (sundayYear /= year)
    monthLab = monthNames !! (sundayMonth - 1) <$ guard (sundayMonth /= month)
    (_, month, _) = Time.toGregorian monday
    (fromIntegral -> sundayYear, sundayMonth, _) = Time.toGregorian sunday
    monday = Time.fromWeekDate (fromIntegral year) week 1
    sunday = Time.fromWeekDate (fromIntegral year) week 7

monthNames :: [String]
monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
