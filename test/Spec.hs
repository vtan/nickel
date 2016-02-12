module Main (main) where

import qualified Gold.Main.Spec

import Test.Hspec

main :: IO ()
main = hspec Gold.Main.Spec.spec
