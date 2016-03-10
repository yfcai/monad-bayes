{-# LANGUAGE
  GADTs,
  Rank2Types
 #-}

module Trace.Debug where

-----------------------
-- DEBUG EXPERIMENTS --
-----------------------

import Control.Exception (assert)
import Control.Monad
import Data.List (unfoldr, intercalate)
import Data.Number.LogFloat hiding (sum)
import Data.Typeable
import System.Random
import Text.Printf

import Base
import Inference
import Primitive
import Sampler
import Trace

import qualified Trace.ByTime as ByTime
import qualified Trace.ByDist as ByDist

mhRun :: (forall m. (MonadBayes m) => m a) -> Int -> Int -> [a]
mhRun = mhRunWith ByTime.empty

mhRunWith :: (RandomDB r) => r -> (forall m. (MonadBayes m) => m a) -> Int -> Int -> [a]
mhRunWith r p seed steps = sample (mh' r steps p) (mkStdGen seed)

-- Print Cache for debugging
instance Show Cache where
  show (Cache (Normal m s) x) = printf "[%.3f<-N%.3f|%.3f]" x m s
  show (Cache (Gamma  a b) x) = printf "[%.3f<-G%.3f|%.3f]" x a b
  show (Cache (Beta   a b) x) = printf "[%.3f<-B%.3f|%.3f]" x a b
  -- print Booleans for debugging
  show (Cache (Categorical ps) x) =
    case cast (x, ps) :: Maybe (Bool, [(Bool, LogFloat)]) of
      Nothing      -> "[cat(" ++ (tail $ init $ show $ map snd ps) ++ ")]"
      Just (x, ps) -> "[" ++ show x ++ "<-(" ++
                         (intercalate "," $ map (\(b,p) -> head (show b) : printf "%.3f" (fromLogFloat p)) ps)
                         ++ ")]"

-- Run a sampler many times to produce many samples.
-- A reference to check MH against.
sampleMany :: Sampler a -> Int -> Int -> [a]
sampleMany sampler seed size = sample (sequence $ replicate size $ sampler) (mkStdGen seed)

mhDebugHistogram :: (RandomDB r) => Histo -> (forall m. (MonadBayes m) => m Double) -> r -> Int -> Int -> IO ()
mhDebugHistogram histo p r0 seed steps = do
  let (samples, acceptance, reuse) = sample (mhWithStats' r0 steps p) (mkStdGen seed)

  assert (steps == length samples) $
    assert (steps == mhTotalSteps acceptance) $
    histogram histo samples

  let meanRatio = mhAcceptanceRatio acceptance

  let acceptRate = fromIntegral (mhAccepted acceptance) / fromIntegral steps :: Double

  let totalNewSize = mhNewSize reuse
  let totalResampled = mhResampled reuse
  let totalReused = totalNewSize - totalResampled
  let reuseRate = fromIntegral totalReused / fromIntegral totalNewSize :: Double

  putStrLn $ printf "  Acceptance rate = %6.2f %%" (100 * acceptRate)
  putStrLn $ printf "Mean accept ratio = %6.2f %%" (100 * meanRatio)
  putStrLn $ printf "       Reuse rate = %6.2f %%" (100 * reuseRate)

data Histo = Histo { xmin :: Double -- lower bound of samples
                   , step :: Double -- size of bins
                   , xmax :: Double -- upper bound of samples
                   , ymax :: Double -- maximum density value to plot as a bar
                   , cols :: Int    -- number of characters in longest bar
                   }

-- | plot @[Double]@ as histogram
histogram :: Histo -> [Double] -> IO ()
histogram (Histo xmin step xmax ymax cols) xs0 =
  let
    -- remove out of bound data from the pool
    xs = filter (\x -> xmin <= x && x <= xmax) xs0
    -- create the next interval starting from x
    --
    --  nextInterval :: Double -> Maybe ((Double -> Bool, Double), Double)
    --                  ^^^^^^            ^^^^^^^^^^^^^^  ^^^^^^   ^^^^^^
    --                  lower bound       interval        middle   upper bound
    nextInterval x = if x >= xmax then
                       Nothing
                     else
                       let y = x + step in Just ((\z -> x <= z && z < y, x + 0.5 * step), y)
    intervals = unfoldr nextInterval xmin
    size = length xs
    bins = map (flip filter xs . fst) intervals
    mids = map snd intervals
    range = step * fromIntegral (length bins)
    -- 1 == sum_{ys in bins} factor * step * (length ys)
    --   == factor * step * size
    factor = 1 / (step * fromIntegral size)
    densities = map (\ys -> factor * fromIntegral (length ys)) bins
    maxDensity = maximum densities
    ymax' = if ymax == 0 && maxDensity == 0 then
              1
            else if ymax == 0 then
              maxDensity
            else
              ymax
    bars = map (\d -> concat $ replicate (min cols $ round (d / ymax' * fromIntegral cols)) "#") densities
    annotatedBars = zipWith3 (printf "%6.3f @ %6.3f %s") densities mids bars
  in
    putStrLn $ unlines annotatedBars

-- Successive lines in do-notation generates right-skewed trace.
-- Example:
--
--   mhDebugHistogram (Histo 1.125 0.25 8.875 0.5 60) gaussians ([]::[Cache]) 0 4000
--
gaussians :: MonadDist m => m Double
gaussians = do
  x <- normal 2 0.70710678118
  y <- normal 3 0.70710678118
  return $ x + y

-- Nested do-notation generates left-subtrees.
-- Example:
--
--   mhDebugHistogram (Histo 0.5125 0.025 1.0875 12 60) deps ([]::[Cache]) 0 4000
--
deps :: MonadDist m => m Double
deps = do
  x <- do
    z <- normal 5 0.6
    gamma z 0.5
  y <- beta x 0.4
  return y

-- Program with a variable number of random choices.
-- Example:
--
--   mhDebugHistogram (Histo (-2.5) 1 27.5 0.35 60) varChoices ([]::[Cache]) 0 4000
--   mhDebugHistogram (Histo (-2.5) 1 27.5 0.35 60) varChoices (ByDist []) 0 4000
--   mhDebugHistogram (Histo (-2.5) 1 27.5 0.35 60) varChoices (ByType []) 0 4000
--   histogram (Histo (-2.5) 1 27.5 0.35 60) $ sampleMany varChoices 0 20000
--
-- Rate of reuse is low because the program makes 1 to 3 random choices
-- most of the time, so x is resampled with significant probability.
varChoices :: MonadDist m => m Double
varChoices = do
  -- Use Gaussian to mimic geometric, since geometric
  -- is implemented in terms of categorical, and we don't
  -- support categorical yet.
  x <- normal 0 5
  let n = floor (abs x / 2)

  xs <- sequence $ replicate n $ normal (abs x) 1
  return $ sum xs

-- Figure 8b of
-- Hur, Nori, Rajamani, Samuel:
-- A provably correct sampler for probabilistic programs.
--
-- Not using fig 2 because I don't know how they parametrize the
-- Gamma distribution. Histogram produced by MH looks okay so far.
-- In fact, it agrees with WolframAlpha better than the histogram
-- in Hur et al. The maximum density of Normal 10 2 is 0.19947,
-- so the maximum density of fig8b on positive reals should be
-- half of that, i. e., around 0.1.
--
-- Examples:
--
--   mhDebugHistogram (Histo (-4.25) 0.25 19.25 0.5 60) fig8b ([]::[Cache]) 0 4000
--   mhDebugHistogram (Histo (-4.25) 0.25 19.25 0.5 60) fig8b (ByDist []) 0 4000
--   histogram (Histo (-4.25) 0.25 19.25 0.5 60) $ sampleMany fig8b 0 10000
--
fig8b :: MonadDist m => m Double
fig8b = do
  x <- normal 0 1
  if x > 0 then
    normal 10 2
  else
    return x

-- Grass model: returning 0 or 1 for histogram.
-- Can only get 1 significant digit in 20k samples;
-- try seeds 0, 300, 1997, 314159.
--
-- Should compare with sampling from posterior.
--
-- Examples:
--
--   mhDebugHistogram (Histo (-0.5) 1.0 1.5 1.0 60) grassModel ([]::[Cache]) 0 4000
--   mhDebugHistogram (Histo (-0.5) 1.0 1.5 1.0 60) grassModel (ByDist []) 0 4000
--   enumerate grassModel -- after import Dist; expect P[0]=0.53, P[1]=0.47
--
grassModel :: MonadBayes m => m Double
grassModel = do
  let flip p  = categorical [(True, p), (False, 1 - p)]
  let m <&& b = liftM2 (&&) m (return b)
  let (<||>)  = liftM2 (||)
  rain       <- flip 0.3
  sprinkler  <- flip 0.5
  grassIsWet <- (flip 0.9 <&& rain)
           <||> (flip 0.8 <&& sprinkler)
           <||> flip 0.1
  condition grassIsWet
  return $ if rain then 1.0 else 0.0
