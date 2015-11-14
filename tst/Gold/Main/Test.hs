{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Gold.Main.Test (spec) where

import Gold.Main

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Time as Time



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

  describe "neighborhoods" $ do

    it "is correct for an example" $
      neighborhoods 1 ([1, 2, 3] :: [Int]) `shouldBe`
      [ [Nothing, Just 1, Just 2]
      , [Just 1, Just 2, Just 3]
      , [Just 2, Just 3, Nothing]
      ]

    prop "has the same length as the given list" $
      \(getNonNegative -> r) (xs :: [Int]) ->
        length (neighborhoods r xs) `shouldBe` length xs

    prop "contains windows with the correct size" $
      \(getNonNegative -> r) (xs :: [Int]) ->
        let n = 2 * r + 1
        in  map length (neighborhoods r xs) `shouldBe` map (const n) xs

instance Arbitrary Expense where
  arbitrary = Expense <$> arbDay <*> arbPos <*> arbStr <*> arbStr
    where
      arbDay = (`Time.addDays` Time.ModifiedJulianDay 0) . getPositive <$> arbitrary
      arbStr = listOf1 $ elements ['a'..'z']
      arbPos = getPositive <$> arbitrary
