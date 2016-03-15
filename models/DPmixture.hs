{-# LANGUAGE
 TupleSections,
 FlexibleContexts
 #-}

module DPmixture (
                  stick,
                  dp,
                  dpMixture,
                  dpMem,
                  dpClusters,
                  dpMemClusters,
                  posteriorClustersDist
                 ) where

-- Dirichlet Process mixture of Gaussians

import Data.Number.LogFloat (LogFloat, logFloat)
import Data.List
import Data.Maybe
import Data.Ix (range)
import Numeric.SpecFunctions (logGamma, factorial)
import Control.Monad.Memo (memo, startEvalMemoT)

import Base
import Primitive
--import Dist

type NormalInvGamma = (Double,Double,Double,Double)

-- | Prior over cluster parameters used in 'dpMixture'
params :: NormalInvGamma
params = (0,1,1,1)
(m,k,a,b) = params

obs :: [Double]
obs = [1.0,1.1,1.2,-1.0,-1.5,-2.0,0.001,0.01,0.005,0.0]

-- | Stick-breaking function.
stick :: MonadDist d => [LogFloat] -> [a] -> d a
stick (b:breaks) (a:atoms) = do
  stop <- bernoulli b
  if stop then return a else stick breaks atoms

-- | A Dirichlet Process generates a random probability distribution
-- using the stick-breaking representation.
dp :: MonadDist d => Double -> d a -> d (d a)
dp concentration base = do
  breaks <- sequence $ repeat $ fmap logFloat $ beta 1 concentration
  atoms  <- sequence $ repeat base
  return $ stick breaks atoms



-- | DP mixture example from http://dl.acm.org/citation.cfm?id=2804317
dpMixture :: MonadBayes d => d [Int]
dpMixture =
  let
    --lazily generate clusters
    clusters = do
      let atoms = [1..]
      breaks <- sequence $ repeat $ fmap logFloat $ beta 1 1
      let classgen = stick breaks atoms
      vars <- sequence $ repeat $ fmap ((1/) . (*k)) $ gamma a b
      means <- mapM (normal m) vars
      return (classgen, vars, means)
    n = length obs
    --start with no data points
    start = fmap (,[]) clusters

    --add points one by one
    points = foldl build start obs
    build d y = do
      (clusters, rest) <- d
      let (classgen, vars, means) = clusters
      cluster <- classgen
      let mean = means !! cluster
      let var  = vars  !! cluster
      let point = (cluster, var, mean)
      observe (Normal mean (sqrt var)) y
      return (clusters, point : rest)

  in
   --exctract cluster assignments
   fmap (reverse . map (\(x,_,_) -> x) . snd) points

-- | 'dpMixture' restricted to the number of clusters only
dpClusters :: MonadBayes d => d Int
dpClusters = fmap (maximum . normalizePartition) dpMixture

-- | Renames identifiers so that they range from 1 to n
normalizePartition :: [Int] -> [Int]
normalizePartition xs = mapMaybe (`lookup` trans) xs where
  trans = zip unique [1..]
  unique = nub xs

-------------------------------------------------------
-- Strict version using memoization

cluster_param :: MonadDist m => Int -> m (LogFloat, Double, Double)
cluster_param _ = do
  b <- beta 1 1
  var <- do
    t <- gamma a b
    return (1 / (k*t))
  mean <- normal m var
  return (logFloat b, mean, var)

dpMem :: MonadBayes m => m [Int]
dpMem = startEvalMemoT $ mapM process_point obs where
  process_point x = do
    (c, mean, var) <- get_cluster
    observe (Normal mean (sqrt var)) x
    return c
  get_cluster = stick 0 where
    stick c = do
      (b, mean, var) <- memo cluster_param c
      stop <- bernoulli b
      if stop then return (c, mean, var) else stick (c+1)

dpMemClusters :: MonadBayes d => d Int
dpMemClusters = fmap (maximum . normalizePartition) dpMem

----------------------------------------------------------
-- Exact posterior

-- | Posterior for parameters of a cluster
posterior :: NormalInvGamma -> [Double] -> NormalInvGamma
posterior (m,k,a,b) xs = (m',k',a',b') where
    m' = (k * m + n * x) / k'
    k' = k + n
    a' = a + n / 2
    b' = b + (m * m * k + x2 - m' * m' * k') / 2 --s / 2 + (k * n * (x - m) ^ 2) / (2 * (k + n))
    n  = fromIntegral $ length xs
    x  = sum xs / n  -- mean xs
    x2 = sum $ zipWith (*) xs xs  -- sum xs^2
    s  = sum $ zipWith (*) d d where  -- pvar xs
            d = map (+ (- x)) xs

-- | Model evidence for a cluster
evidence :: NormalInvGamma -> [Double] -> Double
evidence prior xs = exp (logGamma a' - logGamma a) * (b ** a / b' ** a') * sqrt (k / k') * c ^ n where
    (m,k,a,b)     = prior
    (m',k',a',b') = posterior prior xs
    c             = 1 / (2 * (sqrt pi))
    n             = length xs


-- | CRP prior over partitions
crp :: [Int] -> Double
crp labels = (product $ map factor counts) / factorial n where
    factor x = factorial (x - 1)
    n = length labels
    counts = map (\x -> length (filter (== x) labels)) clusters
    clusters = nub labels

-- | DP mixture likelihood of a partition
mixture :: NormalInvGamma -> [Double] -> [Int] -> Double
mixture prior xs ks = (product $ map (evidence prior) clusters) * crp ks where
  n = length xs
  clusters = map extract labels
  labels = nub ks
  extract n = map fst $ filter ((== n) . snd) points
  points = zip xs ks

-- | Exact posterior for a partition
exact :: NormalInvGamma -> [Double] -> [Int] -> Double
exact prior xs = let
  z = sum $ map (mixture prior xs) (partitions n)
  n = length xs
  f partition = mixture prior xs partition / z
  in
   f

-- | Exact posterior over the number of clusters
exactClusters :: NormalInvGamma -> [Double] -> Int -> Double
exactClusters prior xs = let
  partitionScore = exact prior xs
  results = map score [1..n]
  score k = sum $ map partitionScore $ filter ((== k) . maximum) $ partitions n
  n = length xs
  lookup i = results !! (i - 1)
  in
   lookup

-- | All possible partitions
partitions :: Int -> [[Int]]
partitions n = generate n 1 where
  generate 0 _ = [[]]
  generate n k = do
    x <- range (1,k)          -- range is inclusive on both ends
    let k' = max k (x+1)      -- next empty cluster
    xs <- generate (n-1) k'
    return (x:xs)

-- | Posterior over the number of clusters
posteriorClustersDist :: MonadDist d => d Int
posteriorClustersDist = categorical $ zip ns $ map lookup ns where
    ns = [1 .. length obs]
    lookup = logFloat . exactClusters params obs
