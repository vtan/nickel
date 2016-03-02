{-# LANGUAGE ViewPatterns #-}

module Gold.WeeklyChartSpec (spec) where

import Gold.Account
import Gold.Util
import Gold.WeeklyChart

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

    describe "wdSums" $ do

      it "is empty for empty expenses" $
        wdSums (weeklyData (Week 2000 1) "a" []) `shouldBe` mempty

      prop "starts on the week of the earliest expense" $
        \(arbWeek -> w) (arbExpenses -> exps) ->
          (minimum' . Map.keysSet $ wdSums (weeklyData w "" exps)) `shouldBe`
          (minimum' . map (weekOfDay . expDate) $ exps)

      prop "ends on the week of the last expense" $
        \(arbWeek -> w) (arbExpenses -> exps) ->
          (maximum' . Map.keysSet $ wdSums (weeklyData w "" exps)) `shouldBe`
          (maximum' . map (weekOfDay . expDate) $ exps)

      prop "has the same sum as expenses" $
        \(arbWeek -> w) (arbExpenses -> exps) ->
          (Fold.sum . wdSums $ weeklyData w "" exps) `shouldBe`
          (sum . map expAmount $ exps)

    describe "wdSmoothSums" $ do

      it "is empty for empty expenses" $
        wdSmoothSums (weeklyData (Week 2000 1) "a" []) `shouldBe` mempty

      prop "starts on the week of the earliest expense before the current\
           \ week" $
        \(arbWeek -> w) (arbExpenses -> exps) ->
          (minimum' . Map.keysSet $ wdSmoothSums (weeklyData w "" exps))
          `shouldBe`
          (minimum' . filter (<w) . map (weekOfDay . expDate) $ exps)

      prop "ends one week before the current week" $
        \(arbWeek -> w) (arbExpenses -> exps) ->
          (maximum' . Map.keysSet $ wdSmoothSums (weeklyData w "" exps))
          `shouldBe`
          (pred w <$ guard (not . null . filter (<w)
                            . map (weekOfDay . expDate) $ exps))

      it "smoothes out 1 empty week" $
        let exps = [ Expense (toEnum 0) 10 "" ""
                   , Expense (toEnum 14) 10 "" ""
                   ]
            wd = weeklyData (Week 2010 1) "" exps
        in  (filter (==0) . Map.elems . wdSmoothSums $ wd) `shouldBe` []



newtype ArbYear = ArbYear { arbYear :: Integer } deriving (Show)
instance Arbitrary ArbYear where arbitrary = ArbYear <$> choose (1900, 2100)

newtype ArbMonth = ArbMonth { arbMonth :: Int } deriving (Show)
instance Arbitrary ArbMonth where arbitrary = ArbMonth <$> choose (1, 12)

newtype ArbWeek = ArbWeek { arbWeek :: Week } deriving (Show)
instance Arbitrary ArbWeek where
  arbitrary = ArbWeek . weekOfDay . Time.ModifiedJulianDay . getPositive
          <$> arbitrary

newtype ArbExpenses = ArbExpenses { arbExpenses :: [Expense] } deriving (Show)
instance Arbitrary ArbExpenses where
  arbitrary = ArbExpenses <$> listOf1 genExp
    where
      genExp = Expense <$> genDay <*> genPos <*> genStr <*> genStr
      genDay = Time.ModifiedJulianDay . getPositive <$> arbitrary
      genStr = listOf1 $ elements ['a'..'z']
      genPos = getPositive <$> arbitrary
