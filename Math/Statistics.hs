------------------------------------------------------------------------
-- |
-- Module      :  Math.Statistics
-- Copyright   :  (c) Johan Tibell 2008
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  me@willsewell.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Common statistical functions.
--
------------------------------------------------------------------------

module Math.Statistics
    ( mean
    , median
    , stddev
    , variance
    ) where

import Data.List (sort)

-- | Numerically stable mean.
mean :: Floating a => [a] -> a
mean = go 0 0
    where
      go :: Floating a => a -> Int -> [a] -> a
      go m _ []     = m
      go m n (x:xs) = go (m + (x - m) / (fromIntegral $ n + 1)) (n + 1) xs

-- | Median.
median :: (Floating a, Ord a) => [a] -> a
median xs
    | odd n     = head $ drop (n `div` 2) xs'
    | otherwise = mean $ take 2 $ drop i xs'
    where
      i   = (length xs' `div` 2) - 1
      xs' = sort xs
      n   = length xs

-- | Standard deviation.
stddev :: Floating a => [a] -> a
stddev xs = sqrt $ variance xs

-- | Numerically stable sample variance.
variance :: Floating a => [a] -> a
variance xs = (go 0 0 0 xs) / (fromIntegral $ length xs - 1)
    where
      go :: Floating a => a -> Int -> a -> [a] -> a
      go _ _ s [] = s
      go m n s (x:xs') = go nm (n + 1) (s + delta * (x - nm)) xs'
         where
           delta = x - m
           nm = m + delta / (fromIntegral $ n + 1)
