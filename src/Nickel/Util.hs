module Nickel.Util
  ( tagGroupBy
  , minimum'
  , maximum'
  , minMax'
  , movingAvgs
  , neighborhoods
  ) where

import Data.List
import Data.Map (Map)
import Data.Maybe
import qualified Data.Foldable as Fold
import qualified Data.Map as Map



tagGroupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
tagGroupBy f = Map.fromListWith (++) . map (\x -> (f x, [x]))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' x
  | Fold.null x = Nothing
  | otherwise = Just $ Fold.minimum x

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' x
  | Fold.null x = Nothing
  | otherwise = Just $ Fold.maximum x

minMax' :: (Foldable t, Ord a) => t a -> Maybe (a, a)
minMax' x
  | Fold.null x = Nothing
  | otherwise = Just (Fold.minimum x, Fold.maximum x)

movingAvgs :: Fractional a => Int -> [a] -> [a]
movingAvgs r = map (avg . catMaybes) . neighborhoods r
  where
    avg = (/) <$> sum <*> fromIntegral . length

neighborhoods :: Int -> [a] -> [[Maybe a]]
neighborhoods _ [] = []
neighborhoods radius xs = transpose $ map ($ xs') fs
  where
    xs' = map Just xs
    fs = reverse (map bwdN [1..radius]) ++ [id] ++ map fwdN [1..radius]
    fwdN n = foldr (.) id $ replicate n fwd
    bwdN n = foldr (.) id $ replicate n bwd
    fwd = (++ [Nothing]) . tail
    bwd = (Nothing :) . init
