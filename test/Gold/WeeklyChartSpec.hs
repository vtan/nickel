{-# LANGUAGE ViewPatterns #-}

module Gold.WeeklyChartSpec (spec) where

import Gold.Account
import Gold.WeeklyChart

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Time as Time



spec :: Spec
spec = do

  describe "instance Enum Week" $ do

    describe "fromEnum" $

      prop "is right inverse to toEnum" $ \n ->
        fromEnum (toEnum n :: Week) `shouldBe` n

    describe "toEnum" $

      prop "is right inverse to fromEnum" $ \n ->
        let w = toEnum n :: Week
        in  (toEnum . fromEnum $ w) `shouldBe` w

  describe "fstOfYearOnWeek" $ do

    prop "Just if 1st Jan is that week" $ \(arbYear -> y) ->
      let d = Time.fromGregorian y 1 1
      in  fstOfYearOnWeek (weekOfDay d) `shouldBe` Just d

  describe "fstOfMonthOnWeek" $ do

    prop "Just if the 1st is that week" $ \(arbYear -> y) (arbMonth -> m) ->
      let d = Time.fromGregorian y m 1
      in  fstOfMonthOnWeek (weekOfDay d) `shouldBe` Just d

  describe "weeklyData" $ do

    it "yields empty sums for empty expenses" $
      wdSums (weeklyData (Week 2000 1) "a" []) `shouldBe` mempty

    it "yields empty smooth sums for empty expenses" $
      wdSmoothSums (weeklyData (Week 2000 1) "a" []) `shouldBe` mempty

    prop "the earliest sum is on the week of the earliest expense" $
      \(arbWeek -> w) (map arbExpense . getNonEmpty -> exps) ->
        (fst . Map.findMin $ wdSums (weeklyData w "" exps)) `shouldBe`
        (weekOfDay . minimum . map expDate $ exps)

    prop "the last sum is on the week of the last expense" $
      \(arbWeek -> w) (map arbExpense . getNonEmpty -> exps) ->
        (fst . Map.findMax $ wdSums (weeklyData w "" exps)) `shouldBe`
        (weekOfDay . maximum . map expDate $ exps)

    prop "the earliest smooth sum is on the week of the earliest expense" $
      \(arbWeek -> w) (map arbExpense . getNonEmpty -> exps) ->
        (fst . Map.findMin $ wdSmoothSums (weeklyData w "" exps)) `shouldBe`
        (weekOfDay . minimum . map expDate $ exps)

    prop "the last smooth sum is on the week of the last expense before\
         \ the current week" $
      \(arbWeek -> w) (map arbExpense . getNonEmpty -> exps) ->
        let lastSmoothSum = listToMaybe . Set.toDescList . Map.keysSet
                          $ wdSmoothSums (weeklyData w "" exps)
            lastExpBeforeCur = listToMaybe . sortBy (comparing Down)
                             . filter (<w) . map (weekOfDay . expDate) $ exps
        in  lastSmoothSum `shouldBe` lastExpBeforeCur



newtype ArbYear = ArbYear { arbYear :: Integer } deriving (Show)
instance Arbitrary ArbYear where arbitrary = ArbYear <$> choose (1900, 2100)

newtype ArbMonth = ArbMonth { arbMonth :: Int } deriving (Show)
instance Arbitrary ArbMonth where arbitrary = ArbMonth <$> choose (1, 12)

newtype ArbWeek = ArbWeek { arbWeek :: Week } deriving (Show)
instance Arbitrary ArbWeek where
  arbitrary = ArbWeek . weekOfDay . Time.ModifiedJulianDay . getPositive
          <$> arbitrary

newtype ArbExpense = ArbExpense { arbExpense :: Expense } deriving (Show)
instance Arbitrary ArbExpense where
  arbitrary = ArbExpense <$> arbExp
    where
      arbExp = Expense <$> arbDay <*> arbPos <*> arbStr <*> arbStr
      arbDay = Time.ModifiedJulianDay . getPositive <$> arbitrary
      arbStr = listOf1 $ elements ['a'..'z']
      arbPos = getPositive <$> arbitrary
