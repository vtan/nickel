module Gold.WeeklyChartSpec (spec) where

import Gold.WeeklyChart

import Test.Hspec
import Test.Hspec.QuickCheck



spec :: Spec
spec =

  describe "instance Enum Week" $ do

    prop "fromEnum & toEnum are inverses" $ \n ->
      fromEnum (toEnum n :: Week) `shouldBe` n
{-
  describe "catWeeklySums" $ do

    it "is correct for an example" $
      let e y w cat n = Expense (Time.fromWeekDate y w 1) n "" cat
      in  (Grp.nestedAssocs . catWeeklySums)
            [ e 2000 1 "a" 100, e 2000 1 "a" 250, e 2000 1 "b" 90
            , e 2000 2 "a" 110, e 2000 2 "c" 300
            , e 2000 4 "c" 100
            ]
          `shouldBe`
          [ ("a", [(Week 2000 1, 350), (Week 2000 2, 110)])
          , ("b", [(Week 2000 1, 90)])
          , ("c", [(Week 2000 2, 300), (Week 2000 3, 0), (Week 2000 4, 100)])
          ]
-}

{-
instance Arbitrary Expense where
  arbitrary = Expense <$> arbDay <*> arbPos <*> arbStr <*> arbStr
    where
      arbDay = (`Time.addDays` Time.ModifiedJulianDay 0) . getPositive <$> arbitrary
      arbStr = listOf1 $ elements ['a'..'z']
      arbPos = getPositive <$> arbitrary
-}
