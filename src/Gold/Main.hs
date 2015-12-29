{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Gold.Main where

import Prelude hiding (exp)

import Gold.Grouped (Grouped)
import qualified Gold.Grouped as Grp

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Either
import Data.Function
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Ord
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Control.Lens as Lens
import qualified Data.Colour as Colour
import qualified Data.Colour.Names as Colour
import qualified Data.Default.Class as Default
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec



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

data Expense = Expense
  { expDate :: Time.Day
  , expAmount :: Int
  , expCat :: String
  , expName :: String
  }
  deriving (Eq, Show, Read)

parseExpenses :: [String] -> [Expense]
parseExpenses = snd . partitionEithers . map (Parsec.parse expense "")

expense :: Parsec.Parser Expense
expense = Expense
  <$ Parsec.char '-' <* Parsec.spaces
  <*> date <* Parsec.spaces
  <*> int <* Parsec.spaces
  <*> str <* Parsec.spaces
  <*> str <* Parsec.spaces <* Parsec.eof
  where
    str = Parsec.between quote quote (Parsec.many1 $ Parsec.noneOf "\"")
    quote = Parsec.char '"'
    int = read <$> Parsec.many1 Parsec.digit

date :: Parsec.Parser Time.Day
date = total (Time.fromGregorianValid <$> field 4 <* sep <*> field 2 <* sep <*> field 2)
  where
    field n = read <$> Parsec.count n Parsec.digit
    sep = Parsec.char '-'
    total parser = do
      mx <- parser
      case mx of
        Just x -> pure x
        Nothing -> empty



fillMissingInnerWeeks :: a -> Map Week a -> Map Week a
fillMissingInnerWeeks value weekSums = weekSums `Map.union` zeroes
  where
    zeroes
      | Map.null weekSums = Map.empty
      | otherwise = Map.fromList . map (, value) $ enumFromTo mi ma
    (mi, _) = Map.findMin weekSums
    (ma, _) = Map.findMax weekSums

catWeeklySumsCounts :: [Expense] -> Grouped '[String, Week] (Int, Int)
catWeeklySumsCounts exps =
    Grp.mapGroup (fillMissingInnerWeeks (0, 0))
  . fmap (sum . map expAmount &&& length)
  . Grp.groupBy (yearWeek . expDate)
  . Grp.groupBy expCat
  $ Grp.fromValue exps

yearWeek :: Time.Day -> Week
yearWeek (Time.toWeekDate -> (y, w, _)) = Week (fromIntegral y) w

catWeeklyCharts :: Week -> Grp.Grouped '[String, Week] (Int, Int) -> Chart.StackedLayouts Week
catWeeklyCharts currentWeek catsWeeksSumsCounts = Default.def
  & Lens.set Chart.slayouts_layouts layouts
  where
    layouts = map Chart.StackedLayout charts
    charts = map (uncurry $ weeklyChart currentWeek) orderedCatsWeeksSums
    orderedCatsWeeksSums =
        (map . fmap . fmap) fst
      . sortBy (comparing $ Down . Foldable.sum . fmap snd . snd)
      . Map.assocs $ Grp.groups catsWeeksSumsCounts

weeklyChart :: Week -> String -> Grp.Grouped '[Week] Int -> Chart.Layout Week Int
weeklyChart currentWeek cat weekSums = Default.def
  & Lens.set Chart.layout_title cat
  . Lens.set Chart.layout_plots [dataPlot, avgPlot]
  where
    dataPlot = Chart.plotBars $ Default.def
      & Lens.set Chart.plot_bars_values (zip weeks (map pure values))
      & Lens.set Chart.plot_bars_item_styles [(fillStyle, Nothing)]
    avgPlot = Chart.toPlot $ Default.def
      & Lens.set Chart.plot_lines_values [zip closedWeeks avgs]
      & Lens.set Chart.plot_lines_style lineStyle
    (weeks, values) = unzip $ Grp.nestedAssocs weekSums
    closedWeeks
      | last weeks == currentWeek = init weeks
      | otherwise = weeks
    avgs = map floor . movingAvgs 2 . map fromIntegral $ values
    fillStyle = Chart.FillStyleSolid $ Colour.opaque Colour.lightsteelblue
    lineStyle = Default.def
      & Lens.set Chart.line_color (Colour.opaque Colour.midnightblue)
      & Lens.set Chart.line_width 1.5

movingAvgs :: Fractional a => Int -> [a] -> [a]
movingAvgs r = map (avg . catMaybes) . neighborhoods r
  where
    avg = (/) <$> sum <*> fromIntegral . length

neighborhoods :: Int -> [a] -> [[Maybe a]]
neighborhoods _ [] = []
neighborhoods radius xs = transpose $ map ($ xs') fs
  where
    xs' = map Just xs
    fs = reverse (map bwdN [1..radius]) ++ [id] ++ map fwdN [1..radius]
    fwdN n = foldr (.) id $ replicate n fwd
    bwdN n = foldr (.) id $ replicate n bwd
    fwd = (++ [Nothing]) . tail
    bwd = (Nothing :) . init

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



main :: IO ()
main = do
  content <- readFile "/home/vtan/doc/pez"
  today <- Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime
  let
    sums = catWeeklySumsCounts . parseExpenses . lines $ content
    heightFactor = fromIntegral . Foldable.length . Grp.groups $ sums
    chart = catWeeklyCharts (yearWeek today) sums
    format = Chart.FileOptions (1000, heightFactor * 500) Chart.SVG
  void . Chart.renderableToFile format "weekly.svg" . Chart.toRenderable $ chart
