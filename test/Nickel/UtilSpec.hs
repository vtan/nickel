{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Nickel.UtilSpec (spec) where

import Nickel.TestUtil
import Nickel.Util

import Data.Ratio

import Test.Hspec
import Test.Hspec.QuickCheck



spec :: Spec
spec = do

  describe "neighborhoods" $ do

    it "is correct for an example" $
      neighborhoods 1 ([1, 2, 3] :: [Int]) `shouldBe`
      [ [Nothing, Just 1, Just 2]
      , [Just 1, Just 2, Just 3]
      , [Just 2, Just 3, Nothing]
      ]

    prop "has the same length as the input" $
      forArb nats "window radius" $ \r ->
      forArb (listsOf ints) "input list" $ \xs ->
      length (neighborhoods r xs) `shouldBe` length xs

    prop "contains windows with the correct size" $
      forArb nats "window radius" $ \r ->
      forArb (listsOf ints) "input list" $ \xs ->
      intro (2 * r + 1) "window length" $ \n ->
      map length (neighborhoods r xs) `shouldBe` map (const n) xs

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
      forArb nats "window radius" $ \r ->
      forArb (listsOf rationals) "input list" $ \xs ->
      length (movingAvgs r xs) `shouldBe` length xs
