{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Gold.Main.Test (spec) where

import Gold.Main
import qualified Gold.Grouped as Grp

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Ratio

import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time



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
        [ Expense (Time.fromGregorian 2015 1 1) 103 "ggg" "kkk"
        , Expense (Time.fromGregorian 2015 12 21) 12345 "some thing" "cate  gory"
        ]

  describe "instance Enum Week" $ do

    prop "fromEnum & toEnum are inverses" $ \n ->
      fromEnum (toEnum n :: Week) `shouldBe` n

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

  describe "neighborhoods" $ do

    it "is correct for an example" $
      neighborhoods 1 ([1, 2, 3] :: [Int]) `shouldBe`
      [ [Nothing, Just 1, Just 2]
      , [Just 1, Just 2, Just 3]
      , [Just 2, Just 3, Nothing]
      ]

    prop "has the same length as the input" $
      \(getNonNegative -> r) (xs :: [Int]) ->
        length (neighborhoods r xs) `shouldBe` length xs

    prop "contains windows with the correct size" $
      \(getNonNegative -> r) (xs :: [Int]) ->
        let n = 2 * r + 1
        in  map length (neighborhoods r xs) `shouldBe` map (const n) xs

  describe "movingAvgs" $ do

    it "is correct for an example" $
      movingAvgs 2 ([2, 4, 7, 3, 5] :: [Rational]) `shouldBe`
      [ (2 + 4 + 7) % 3
      , (2 + 4 + 7 + 3) % 4
      , (2 + 4 + 7 + 3 + 5) % 5
      , (4 + 7 + 3 + 5) % 4
      , (7 + 3 + 5) % 3
      ]

    prop "has the same length as the input" $
      \(getNonNegative -> r) (xs :: [Rational]) ->
        length (movingAvgs r xs) `shouldBe` length xs



instance Arbitrary Expense where
  arbitrary = Expense <$> arbDay <*> arbPos <*> arbStr <*> arbStr
    where
      arbDay = (`Time.addDays` Time.ModifiedJulianDay 0) . getPositive <$> arbitrary
      arbStr = listOf1 $ elements ['a'..'z']
      arbPos = getPositive <$> arbitrary
