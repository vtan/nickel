module Main (main) where

import qualified Gold.Main.Test

import Test.Hspec

main :: IO ()
main = hspec Gold.Main.Test.spec
