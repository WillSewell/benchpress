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
-- execution time percentiles.  There are functions to pretty-print
-- the results.
--
-- Here's an example showing a benchmark of copying a file:
--
-- > import Control.Monad.Trans
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
-- > main = bench 100 $ liftIO $ do
-- >          inf <- openBinaryFile inpath ReadMode
-- >          outf <- openBinaryFile outpath WriteMode
-- >          copyUsingByteString inf outf
-- >          hClose outf
-- >          hClose inf
--
------------------------------------------------------------------------

module Test.BenchPress
    ( -- * The 'Benchmark' type
      Benchmark,
      start,
      stop,

      -- * Running a benchmark
      benchmark,
      bench,
      benchMany,

      -- * Benchmark stats
      Stats(..),
    ) where

import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (intercalate, sort)
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified Math.Statistics as Math
import Prelude hiding (max, min)
import qualified Prelude
import Text.Printf (printf)

-- ---------------------------------------------------------------------
-- The Benchmark monad

-- | The benchmark state.
data S = S
    {-# UNPACK #-} !NominalDiffTime  -- elapsed time
    {-# UNPACK #-} !(Maybe UTCTime)  -- last mark

initState :: S
initState = S 0 Nothing
{-# INLINE initState #-}

-- | Starts the benchmark timer again after it has been previously
-- stopped by calling 'stop'.  It's not necessary to call 'start' at
-- the beginning of the action you want to benchmark as it is done
-- automatically by 'benchmark'.
start :: Benchmark ()
start = Benchmark $ \(S elapsed _) -> do
          mark <- liftIO getCurrentTime
          return ((), S elapsed (Just mark))
{-# INLINE start #-}

-- | Stops the benchmark timer.  Stopping the timer is useful when you
-- need to perform some set up that you don't want to count in the
-- benchmark timings.  It's not necessary to call 'stop' at the end of
-- the action you want to benchmark as it is done automatically by
-- 'benchmark'.
stop :: Benchmark ()
stop = Benchmark $ \(S elapsed (Just mark)) -> do
         now <- liftIO getCurrentTime
         return ((), S (elapsed + (diffUTCTime now mark)) Nothing)
{-# INLINE stop #-}

newtype Benchmark a = Benchmark { runBenchmark :: S -> IO (a, S) }

execBenchmark :: Benchmark a -> S -> IO S
execBenchmark m s = do
  (_, s') <- runBenchmark m s
  return s'

instance Monad Benchmark where
    return a = Benchmark $ \s -> return (a, s)
    {-# INLINE return #-}

    m >>= k = Benchmark $ \s -> do
                           (a, s') <- runBenchmark m s
                           runBenchmark (k a) s'
    {-# INLINE (>>=) #-}

    fail str = Benchmark $ \_ -> fail str

instance MonadIO Benchmark where
    liftIO m = Benchmark $ \s -> m >>= \a -> return (a, s)
    {-# INLINE liftIO #-}

-- ---------------------------------------------------------------------
-- Running a benchmark

-- TODO: Make sure that iters is > 0.

-- | @benchmark iters bm@ runs the action @bm@ @iters@ times measuring
-- the execution time of each run.
benchmark :: Int -> Benchmark a -> IO Stats
benchmark iters ma = do
  timings <- (map millis) `fmap` go iters
  let xs = sort timings
  return Stats
             { min         = head xs
             , mean        = Math.mean xs
             , stddev      = Math.stddev xs
             , median      = Math.median xs
             , max         = last xs
             , percentiles = percentiles' xs
             }
      where
        go 0 = return []
        go n = do (S elapsed _) <- execBenchmark (start >> ma >> stop) initState
                  timings <- go $! n - 1
                  return $! elapsed : timings

-- | Convenience function that runs a benchmark using 'benchmark' and
-- prints timing statistics.
bench :: Int -> Benchmark a -> IO ()
bench iters bm = do
  stats <- benchmark iters bm
  let colWidth = columnWidth [stats]
  printSummaryHeader 0 colWidth
  printSummary colWidth "" stats
  putStrLn ""
  let psTbl = unlines $ columns (percentiles stats)
  putStrLn "Percentiles (ms)"
  putStr psTbl
    where
      columns = map $ \(p, value) -> printf " %3d%%  %5.3f" p value

-- | Convenience function that runs several benchmarks using
-- 'benchmark' and prints a timing statistics summary.  Each benchmark
-- has an associated label that is used to identify the benchmark in
-- the printed results.
benchMany :: Int -> [(String, Benchmark a)] -> IO ()
benchMany iters bms = do
  results <- mapM (benchmark iters . snd) bms
  let lblLen = maximum (map (length . fst) bms) + 2
      colWidth = columnWidth results
  printSummaryHeader lblLen colWidth
  forM_ (zip (map fst bms) results) $ \(label, stats) ->
      printSummary colWidth (printf "%-*s" lblLen (label ++ ": ")) stats
  return ()

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
-- Benchmark stats

-- | Timing statistics for the benchmark.  All measured times are
-- given in milliseconds.
data Stats = Stats
    { min         :: Double
    -- ^ Shortest execution time.
    , mean        :: Double
    -- ^ Average execution time.
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
    } deriving (Eq, Show)

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

-- | Convert microseconds to milliseconds.
millis :: NominalDiffTime -> Double
millis t = realToFrac t * (10^(3 :: Int))
