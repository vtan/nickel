module Gold.Util where

import Data.List
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map



tagGroupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
tagGroupBy f = Map.fromListWith (++) . map (\x -> (f x, [x]))

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
