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

instance Arbitrary Expense where
  arbitrary = Expense <$> arbDay <*> arbPos <*> arbStr <*> arbStr
    where
      arbDay = (`Time.addDays` Time.ModifiedJulianDay 0) . getPositive <$> arbitrary
      arbStr = listOf1 $ elements ['a'..'z']
      arbPos = getPositive <$> arbitrary
