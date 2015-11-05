{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main, spec) where

import Prelude hiding (exp, sum)

import Grouped (Grouped)
import qualified Grouped as Grp

import Control.Arrow
import Control.Monad
import Data.Either
import Data.Function
import Data.List
import qualified Data.Foldable as Fol
import qualified Data.Map as Map

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary, arbitrary, elements, getPositive, listOf1)
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



type Year = Int
type Month = Int
type Week = Int
type Day = Int

data Expense = Expense
  { expDate :: (Year, Month, Day)
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

date :: Parsec.Parser (Year, Month, Day)
date = (,,) <$> field 4 <* sep <*> field 2 <* sep <*> field 2
  where
    field n = read <$> Parsec.count n Parsec.digit
    sep = Parsec.char '-'



catWeeklySums :: [Expense] -> Grouped '[String, (Year, Week)] Int
catWeeklySums exps =
    fmap (sum . map expAmount)
  . Grp.groupBy yearWeek
  . Grp.groupBy expCat
  $ Grp.fromValue exps
  where
    yearWeek (expDate -> (y, m, d)) =
      let (y', w, _) = Time.toWeekDate $ Time.fromGregorian (fromIntegral y) m d
      in  (fromIntegral y', w)

catWeeklyCharts :: Grp.Grouped '[String, (Year, Week)] Int -> Chart.StackedLayouts Time.LocalTime
catWeeklyCharts cats = Default.def
  & Lens.set Chart.slayouts_layouts layouts
  where
    layouts = map (Chart.StackedLayout . uncurry weeklyChart) . Map.assocs $ Grp.groups cats

weeklyChart :: String -> Grp.Grouped '[(Year, Week)] Int -> Chart.Layout Time.LocalTime Int
weeklyChart cat weeks = Default.def
  & Lens.set Chart.layout_title cat
  . Lens.set Chart.layout_plots [plot]
  where
    plot = Chart.plotBars $ Default.def
      & Lens.set Chart.plot_bars_values values
      & Lens.set Chart.plot_bars_item_styles [(fillStyle, Nothing)]
    values = map (ywToLocalTime *** pure) $ Grp.nestedAssocs weeks
    fillStyle = Chart.FillStyleSolid $ Colour.opaque Colour.steelblue
    ywToLocalTime (y, w) =
      Time.LocalTime (Time.fromWeekDate (fromIntegral y) w 4) Time.midday



main :: IO ()
main = do
  content <- readFile "/home/vtan/doc/pez"
  let
    sums = catWeeklySums . parseExpenses . lines $ content
    heightFactor = fromIntegral . Fol.length . Grp.groups $ sums
    chart = catWeeklyCharts sums
    format = Default.def & Lens.set Chart.fo_size (1000, heightFactor * 500)
  void . Chart.renderableToFile format "weekly.svg" . Chart.toRenderable $ chart

spec :: Spec
spec = do
  describe "parseExpenses" $ do
    it "returns well-formed expenses for a sample input" $
      parseExpenses
        [ "exp 2015-01-01, 100, ggg, kkk"
        , "exp: 2015-01-01, 101, ggg"
        , "exp: 2015-1-01, 102, ggg, kkk"
        , "exp: 2015-01-01, 103, ggg, kkk"
        , "exp:2015-12-21,   12345, some thing,cate  gory"
        , "exp: 2015-01-01, 100, ggg, kkk, lll"
        ]
      `shouldBe`
        [ Expense (2015, 1, 1) 103 "ggg" "kkk"
        , Expense (2015, 12, 21) 12345 "some thing" "cate  gory"
        ]

instance Arbitrary Expense where
  arbitrary = Expense <$> arbPosTriple <*> arbPos <*> arbStr <*> arbStr
    where
      arbStr = listOf1 $ elements ['a'..'z']
      arbPos = getPositive <$> arbitrary
      arbPosTriple = (,,) <$> arbPos <*> arbPos <*> arbPos
