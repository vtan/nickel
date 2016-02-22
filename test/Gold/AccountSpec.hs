module Gold.AccountSpec (spec) where

import Gold.Account

import Test.Hspec
import qualified Data.Time as Time



spec :: Spec
spec =

  describe "parseExpenses" $ do

    it "parses incomes" $
      parseAccounts
        [ "+ 2015-01-01 101"
        , "+ 2015-1-01 102 \"ggg\""
        , "+ 2015-01-01 103 \"ggg\""
        , "+2015-12-21   12345 \"some thing\""
        , "+ 2015-01-01 100 \"ggg\" \"kkk\""
        ]
      `shouldBe`
        [ InvalidLine 1
        , InvalidLine 2
        , ParsedIncome $ Income
            (Time.fromGregorian 2015 1 1) 103 "ggg"
        , ParsedIncome $ Income
            (Time.fromGregorian 2015 12 21) 12345 "some thing"
        , InvalidLine 5
        ]

    it "parses expenses" $
      parseAccounts
        [ "- 2015-01-01 101 \"ggg\""
        , "- 2015-1-01 102 \"ggg\" \"kkk\""
        , "- 2015-01-01 103 \"ggg\" \"kkk\""
        , "-2015-12-21   12345 \"some thing\"   \"cate  gory\""
        , "- 2015-01-01 100 \"ggg\" \"kkk\" \"lll\""
        ]
      `shouldBe`
        [ InvalidLine 1
        , InvalidLine 2
        , ParsedExpense $ Expense
            (Time.fromGregorian 2015 1 1) 103 "ggg" "kkk"
        , ParsedExpense $ Expense
            (Time.fromGregorian 2015 12 21) 12345 "some thing" "cate  gory"
        , InvalidLine 5
        ]
