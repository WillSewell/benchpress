------------------------------------------------------------------------
-- |
-- Module      :  Test.BenchPress
-- Copyright   :  (c) Johan Tibell 2008
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Benchmarks actions and produces statistics such as min, mean,
-- median, standard deviation, and max execution time.  Also computes
-- execution time percentiles.  Comes with functions to pretty-print
-- the results.
--
-- Here's an example showing a benchmark of copying a file:
--
-- > import qualified Data.ByteString as B
-- > import System.IO
-- > import Test.BenchPress
-- >
-- > inpath, outpath :: String
-- > inpath = "/tmp/infile"
-- > outpath = "/tmp/outfile"
-- >
-- > blockSize :: Int
-- > blockSize = 4 * 1024
-- >
-- > copyUsingByteString :: Handle -> Handle -> IO ()
-- > copyUsingByteString inf outf = go
-- >     where
-- >       go = do
-- >         bs <- B.hGet inf blockSize
-- >         let numRead = B.length bs
-- >         if numRead > 0
-- >            then B.hPut outf bs >> go
-- >            else return ()
-- >
-- > main :: IO ()
-- > main = bench 100 $ do
-- >          inf <- openBinaryFile inpath ReadMode
-- >          outf <- openBinaryFile outpath WriteMode
-- >          copyUsingByteString inf outf
-- >          hClose outf
-- >          hClose inf
--
------------------------------------------------------------------------

module Test.BenchPress
    ( -- * Running a benchmark
      benchmark,
      bench,
      benchMany,

      -- * Benchmark stats
      Stats(..),

      -- * Pretty-printing stats
      printDetailedStats,
      printStatsSummaries,
    ) where

import Control.Exception (bracket)
import Control.Monad (forM, forM_)
import Data.List (intersperse, sort)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified Math.Statistics as Math
import Prelude hiding (max, min)
import qualified Prelude
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- ---------------------------------------------------------------------
-- Running a benchmark

-- TODO: Make sure that iters is > 0.

-- | @benchmark iters setup teardown action@ runs @action@ @iters@
-- times measuring the execution time of each run.  @setup@ and
-- @teardown@ are run before and after each run respectively.
-- @teardown@ is run even if @action@ raises an exception.  Returns
-- statistics for both the measured CPU times and wall clock times, in
-- that order.
benchmark :: Int -> IO a -> (a -> IO b) -> (a -> IO c) -> IO (Stats, Stats)
benchmark iters setup teardown action = do
  (cpuTimes, wallTimes) <- unzip `fmap` go iters
  let xs        = sort cpuTimes
      cpuStats  = Stats
                  { min         = head xs
                  , mean        = Math.mean xs
                  , stddev      = Math.stddev xs
                  , median      = Math.median xs
                  , max         = last xs
                  , percentiles = percentiles' xs
                  }
      ys        = sort wallTimes
      wallStats = Stats
                 { min         = head ys
                 , mean        = Math.mean ys
                 , stddev      = Math.stddev ys
                 , median      = Math.median ys
                 , max         = last ys
                 , percentiles = percentiles' ys
                 }
  return (cpuStats, wallStats)
      where
        go 0 = return []
        go n = do
          elapsed <- bracket setup teardown $ \a -> do
            startWall <- getCurrentTime
            startCpu <- getCPUTime
            action a
            endCpu <- getCPUTime
            endWall <- getCurrentTime
            return (picosToMillis $! endCpu - startCpu
                   ,secsToMillis $! endWall `diffUTCTime` startWall)
          timings <- go $! n - 1
          return $ elapsed : timings

-- | Convenience function that runs a benchmark using 'benchmark' and
-- prints timing statistics using 'printDetailedStats'.  The
-- statistics are computed from the measured CPU times.  Writes output
-- to standard output.
bench :: Int -> IO a -> IO ()
bench iters action = do
  (_, stats) <- benchmark iters (return ()) (const $ return ()) (const action)
  printDetailedStats stats

-- | Convenience function that runs several benchmarks using
-- 'benchmark' and prints a timing statistics summary using
-- 'printStatsSummaries'.  The statistics are computed from the
-- measured CPU times.  Each benchmark has an associated label that is
-- used to identify the benchmark in the printed results.  Writes
-- output to standard output.
benchMany :: Int -> [(String, IO a)] -> IO ()
benchMany iters bms = do
  results <- forM bms $ \(_, action) ->
             benchmark iters (return ()) (const $ return ()) (const action)
  printStatsSummaries $ zip (map fst bms) (map fst results)

-- ---------------------------------------------------------------------
-- Benchmark stats

-- | Execution time statistics for a benchmark.  All measured times
-- are given in milliseconds.
data Stats = Stats
    { min         :: Double
    -- ^ Shortest execution time.
    , mean        :: Double
    -- ^ Mean execution time.
    , stddev      :: Double
    -- ^ Execution time standard deviation.
    , median      :: Double
    -- ^ Median execution time.
    , max         :: Double
    -- ^ Longest execution time.
    , percentiles :: [(Int, Double)]
    -- ^ Execution time divided into percentiles.  The first component
    -- of the pair is the percentile given as an integer between 0 and
    -- 100, inclusive.  The second component is the execution time of
    -- the slowest iteration within the percentile.
    } deriving Show

-- ---------------------------------------------------------------------
-- Pretty-printing stats

-- | Prints detailed statistics.  Printed statistics include min,
-- mean, standard deviation, median, and max execution time.  Also
-- prints execution time percentiles.  Writes output to standard
-- output.
printDetailedStats :: Stats -> IO ()
printDetailedStats stats = do
  printSummaryHeader 0 colWidth
  printSummary colWidth "" stats
  putStrLn ""
  putStrLn "Percentiles (ms)"
  putStr psTbl
    where
      columns  = map $ \(p, value) -> printf " %3d%%  %5.3f" p value
      colWidth = columnWidth [stats]
      psTbl    = unlines $ columns (percentiles stats)

-- | Prints a summary row for each benchmark with an associated label.
-- The summary contains the same statistics as in 'printDetailedStats'
-- except for the execution time percentiles.  Writes output to
-- standard output.
printStatsSummaries :: [(String, Stats)] -> IO ()
printStatsSummaries rows = do
  printSummaryHeader lblLen colWidth
  forM_ rows $ \(label, stats) ->
      printSummary colWidth (printf "%-*s" lblLen (label ++ ": ")) stats
    where
      labels   = map fst rows
      results  = map snd rows
      lblLen   = maximum (map length labels) + 2
      colWidth = columnWidth results

-- | Column headers.
headers :: [String]
headers = ["min", "mean", "+/-sd", "median", "max"]

-- | Computes the minimum column width needed to print the results
-- table.
columnWidth :: [Stats] -> Int
columnWidth = Prelude.max (maximum $ map length headers) . maximum . map width
    where
      width (Stats min' mean' sd median' max' _) =
          maximum $ map (length . (printf "%.3f" :: Double -> String))
                      [min', mean', sd, median', max']

-- | Pad header with spaces up till desired width.
padHeader :: Int -> String -> String
padHeader w s
    | n > w       = s
    | odd (w - n) = replicate (amt + 1) ' ' ++ s ++ replicate amt ' '
    | otherwise   = replicate amt ' ' ++ s ++ replicate amt ' '
    where
      n   = length s
      amt = (w - n) `div` 2

-- | Print table headers.
printSummaryHeader :: Int -> Int -> IO ()
printSummaryHeader lblLen colWidth = do
  putStrLn "Times (ms)"
  putStr $ (replicate lblLen ' ') ++ " "
  putStrLn $ intercalate "  " $ map (padHeader colWidth) headers

-- | Print a row showing a summary of the given stats.
printSummary :: Int -> String -> Stats -> IO ()
printSummary w label (Stats min' mean' sd median' max' _) =
    putStrLn $ printf "%s %*.3f  %*.3f  %*.3f  %*.3f  %*.3f"
             label w min' w mean' w sd w median' w max'

-- ---------------------------------------------------------------------
-- Computing statistics

-- | Compute percentiles given a list of execution times in ascending
-- order.
percentiles' :: [Double] -> [(Int, Double)]
percentiles' xs = zipWith (\p ys -> (p, ys !! (rank p))) ps (repeat xs)
    where
      n      = length xs
      rank p = ceiling ((fromIntegral n / 100) * fromIntegral p :: Double) - 1
      ps     = [50, 66, 75, 80, 90, 95, 98, 99, 100]

-- ---------------------------------------------------------------------
-- Internal utilities

-- | Converts picoseconds to milliseconds.
picosToMillis :: Integer -> Double
picosToMillis t = realToFrac t / (10^(9 :: Int))

-- | Converts seconds to milliseconds.
secsToMillis :: NominalDiffTime -> Double
secsToMillis t = realToFrac t * (10^(3 :: Int))

intercalate :: [a] -> [[a]] -> [a]
intercalate xs = concat . intersperse xs
