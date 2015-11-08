{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Gold.Main where

import Prelude hiding (exp, sum)

import Gold.Grouped (Grouped)
import qualified Gold.Grouped as Grp

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Either
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Foldable as Fol
import qualified Data.Map as Map

import qualified Control.Lens as Lens
import qualified Data.Colour as Colour
import qualified Data.Colour.Names as Colour
import qualified Data.Default.Class as Default
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Chart
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec



data Week = Week Int Int
  deriving (Eq, Ord, Show, Read)

data Expense = Expense
  { expDate :: Time.Day
  , expAmount :: Int
  , expName :: String
  , expCat :: String
  }
  deriving (Eq, Show, Read)

parseExpenses :: [String] -> [Expense]
parseExpenses = snd . partitionEithers . map (Parsec.parse expense "")

expense :: Parsec.Parser Expense
expense = Expense
  <$ Parsec.string "exp:" <* Parsec.spaces
  <*> date <* sep
  <*> int <* sep
  <*> str <* sep
  <*> str <* Parsec.spaces <* Parsec.eof
  where
    sep = Parsec.spaces <* Parsec.char ',' <* Parsec.spaces
    str = Parsec.many1 $ Parsec.noneOf ","
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



catWeeklySums :: [Expense] -> Grouped '[String, Week] Int
catWeeklySums exps =
    fmap (sum . map expAmount)
  . Grp.groupBy yearWeek
  . Grp.groupBy expCat
  $ Grp.fromValue exps
  where
    yearWeek (expDate -> day) =
      let (y', w, _) = Time.toWeekDate day
      in  Week (fromIntegral y') w

catWeeklyCharts :: Grp.Grouped '[String, Week] Int -> Chart.StackedLayouts Week
catWeeklyCharts cats = Default.def
  & Lens.set Chart.slayouts_layouts layouts
  where
    layouts = map (Chart.StackedLayout . uncurry weeklyChart) . Map.assocs $ Grp.groups cats

weeklyChart :: String -> Grp.Grouped '[Week] Int -> Chart.Layout Week Int
weeklyChart cat weeks = Default.def
  & Lens.set Chart.layout_title cat
  . Lens.set Chart.layout_plots [plot]
  where
    plot = Chart.plotBars $ Default.def
      & Lens.set Chart.plot_bars_values values
      & Lens.set Chart.plot_bars_item_styles [(fillStyle, Nothing)]
    values = map (id *** pure) $ Grp.nestedAssocs weeks
    fillStyle = Chart.FillStyleSolid $ Colour.opaque Colour.steelblue

instance Chart.PlotValue Week where
  toValue (Week y w) =
    let day = Time.fromWeekDate (fromIntegral y) w 1
    in  fromIntegral $ Time.toModifiedJulianDay day

  fromValue x =
    let (y, w, _) = Time.toWeekDate . Time.ModifiedJulianDay $ floor x
    in  Week (fromIntegral y) w

  autoAxis weeks = Chart.makeAxis (const "") (weeks, [], []) &
    Lens.set Chart.axis_labels (transpose $ map label weeks)
    where
      label yw@(weekLabel -> (yearLab, monthLab)) =
        let yearStr = fromMaybe "" yearLab
            monthStr = fromMaybe "" monthLab
        in  [(yw, monthStr), (yw, yearStr)]

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
  let
    sums = catWeeklySums . parseExpenses . lines $ content
    heightFactor = fromIntegral . Fol.length . Grp.groups $ sums
    chart = catWeeklyCharts sums
    format = Default.def & Lens.set Chart.fo_size (1000, heightFactor * 500)
  void . Chart.renderableToFile format "weekly.svg" . Chart.toRenderable $ chart
