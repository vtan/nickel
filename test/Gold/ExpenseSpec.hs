module Gold.ExpenseSpec (spec) where

import Gold.Expense

import Test.Hspec
import qualified Data.Time as Time



spec :: Spec
spec =

  describe "parseExpenses" $

    it "returns well-formed expenses for a sample input" $
      parseExpenses
        [ "- 2015-01-01 101 \"ggg\""
        , "- 2015-1-01 102 \"ggg\" \"kkk\""
        , "- 2015-01-01 103 \"ggg\" \"kkk\""
        , "-2015-12-21   12345 \"some thing\"   \"cate  gory\""
        , "- 2015-01-01 100 \"ggg\" \"kkk\" \"lll\""
        ]
      `shouldBe`
        [ Expense (Time.fromGregorian 2015 1 1) 103 "ggg" "kkk"
        , Expense (Time.fromGregorian 2015 12 21) 12345 "some thing" "cate  gory"
        ]
