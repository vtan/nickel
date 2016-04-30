{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Nickel.WeeklyChartSpec (spec) where

import Prelude hiding (exp)

import Nickel.Account
import Nickel.TestUtil
import Nickel.Util
import Nickel.WeeklyChart

import Control.Monad
import qualified Data.Foldable as Fold
import qualified Data.Map as Map

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Time as Time



spec :: Spec
spec = do

  describe "instance Enum Week" $ do

    describe "fromEnum" $

      prop "is right inverse to toEnum" $
        forArb ints "n" $ \n ->
        fromEnum (toEnum n :: Week) `shouldBe` n

    describe "toEnum" $

      prop "is right inverse to fromEnum" $
        forArb ints "n" $ \n ->
        intro (toEnum n :: Week) "week" $ \w ->
        (toEnum . fromEnum $ w) `shouldBe` w

  describe "fstOfYearOnWeek" $ do

    prop "Just if 1st Jan is that week" $
      forArb years "year" $ \y ->
      intro (Time.fromGregorian y 1 1) "1st Jan" $ \d ->
      fstOfYearOnWeek (weekOfDay d) `shouldBe` Just d

  describe "fstOfMonthOnWeek" $ do

    prop "Just if the 1st is that week" $
      forArb years "year" $ \y ->
      forArb months "month" $ \m ->
      intro (Time.fromGregorian y m 1) "1st of month" $ \d ->
      fstOfMonthOnWeek (weekOfDay d) `shouldBe` Just d

  describe "weeklyData" $ do

    describe "wdSums" $ do

      it "is empty for empty expenses" $
        wdSums (weeklyData (Week 2000 1) []) `shouldBe` mempty

      prop "starts on the week of the earliest expense" $
        forArb weeks "current week" $ \w ->
        forArb (nonEmptyListsOf expenses) "exps" $ \exps ->
        intro (Map.keysSet $ wdSums (weeklyData w exps))
              "weeks of wdSums" $ \weeksSums ->
        minimum' weeksSums `shouldBe`
        (minimum' . map (weekOfDay . expDate) $ exps)

      prop "ends on the week of the last expense" $
        forArb weeks "current week" $ \w ->
        forArb (nonEmptyListsOf expenses) "exps" $ \exps ->
        intro (Map.keysSet $ wdSums (weeklyData w exps))
              "weeks of wdSums" $ \weeksSums ->
        maximum' weeksSums `shouldBe`
        (maximum' . map (weekOfDay . expDate) $ exps)

      prop "has the same sum as expenses" $
        forArb weeks "current week" $ \w ->
        forArb (nonEmptyListsOf expenses) "exps" $ \exps ->
        (Fold.sum . wdSums $ weeklyData w exps) `shouldBe`
        (sum . map expAmount $ exps)

    describe "wdSmoothSums" $ do

      it "is empty for empty expenses" $
        wdSmoothSums (weeklyData (Week 2000 1) []) `shouldBe` mempty

      prop "starts on the week of the earliest expense before the current\
           \ week" $
        forArb weeks "current week" $ \w ->
        forArb (nonEmptyListsOf expenses) "exps" $ \exps ->
        intro (Map.keysSet . wdSmoothSums $ weeklyData w exps)
              "weeks of wdSmoothSums" $ \weeksSmSums ->
        minimum' weeksSmSums `shouldBe`
        (minimum' . filter (<w) . map (weekOfDay . expDate) $ exps)

      prop "ends one week before the current week" $
        forArb weeks "current week" $ \w ->
        forArb (nonEmptyListsOf expenses) "exps" $ \exps ->
        intro (Map.keysSet . wdSmoothSums $ weeklyData w exps)
              "weeks of wdSmoothSums" $ \weeksSmSums ->
        maximum' weeksSmSums
        `shouldBe`
        (pred w <$
          guard (not . null . filter (<w) . map (weekOfDay . expDate) $ exps))

      it "smoothes out 1 empty week" $
        let exps = [ Expense (toEnum 0) 10 "" ""
                   , Expense (toEnum 14) 10 "" ""
                   ]
            wd = weeklyData (weekOfDay $ toEnum 21) exps
        in  (filter (==0) . Map.elems . wdSmoothSums $ wd) `shouldBe` []



years :: Arb Integer
years = arbNoShrink $ fmap (1858+) arbitrarySizedNatural

months :: Arb Int
months = arbNoShrink $ choose (1, 12)

weeks :: Arb Week
weeks = arbNoShrink $
  weekOfDay . Time.ModifiedJulianDay <$> arbitrarySizedNatural

expenses :: Arb Expense
expenses = Arb{..}
  where
    arbGen = Expense <$> genDay <*> genPos <*> genStr <*> genStr
    arbShrink Expense{ expName = "", expCat = "" } = []
    arbShrink exp = [exp { expName = "", expCat = "" }]
    genDay = Time.ModifiedJulianDay . getPositive <$> arbitrary
    genStr = listOf1 $ elements ['a'..'z']
    genPos = getPositive <$> arbitrary
