{-# LANGUAGE RecordWildCards #-}

module Nickel.TestUtil where

import Test.QuickCheck
import Test.QuickCheck.Property



data Arb a = Arb
  { arbGen :: Gen a
  , arbShrink :: a -> [a]
  }

arbArbitrary :: Arbitrary a => Arb a
arbArbitrary = Arb
  { arbGen = arbitrary
  , arbShrink = shrink
  }

arbArbitraryIso :: Arbitrary a => (a -> b) -> (b -> a) -> Arb b
arbArbitraryIso f g = Arb
  { arbGen = fmap f arbitrary
  , arbShrink = map f . shrink . g
  }

arbNoShrink :: Gen a -> Arb a
arbNoShrink x = Arb
  { arbGen = x
  , arbShrink = const []
  }

forArb :: (Show a, Testable p) => Arb a -> String -> (a -> p) -> Property
forArb Arb{..} name prop = MkProperty $ do
  x <- arbGen
  unProperty . shrinking arbShrink x $ \x' ->
    counterexample (name ++ ": " ++ show x') (prop x')

intro :: (Show a, Testable p) => a -> String -> (a -> p) -> Property
intro x name prop = counterexample (name ++ ": " ++ show x) (prop x)

ints :: Arb Int
ints = arbArbitrary

nats :: Arb Int
nats = arbArbitraryIso getNonNegative NonNegative

rationals :: Arb Rational
rationals = arbArbitrary

listsOf :: Arb a -> Arb [a]
listsOf Arb{..} = Arb
  { arbGen = listOf arbGen
  , arbShrink = shrinkList arbShrink
  }

nonEmptyListsOf :: Arb a -> Arb [a]
nonEmptyListsOf Arb{..} = Arb
  { arbGen = listOf1 arbGen
  , arbShrink = filter (not . null) . shrinkList arbShrink
  }
