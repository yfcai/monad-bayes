{-# LANGUAGE
  ImpredicativeTypes,
  ScopedTypeVariables
 #-}

-- Import all models under maintenance.
-- Models not imported here will not be compiled
-- when invoking `stack bench`.
import qualified BetaBin
import qualified Dice
import qualified DPmixture
import qualified Gamma
import qualified HMM

import Control.Monad.Par.Class (NFData)
import Criterion.Main
import Criterion.Main.Options
import Criterion.Types
import Data.List (intercalate, transpose)
import Data.Maybe
import Data.Number.LogFloat hiding (sum)
import Data.Random (RVar, sampleState)
import Data.Typeable
import Options.Applicative (execParser)
import System.IO
import System.Random (StdGen, mkStdGen, split)
import Text.Printf

import Base
import Dist
import Empirical
import Inference
import Metrics
import Particle (ParticleT)
import Sampler
import qualified Trace.ByDist as ByDist
import qualified Trace.ByTime as ByTime
import qualified Trace.ByType as ByType

-- | Terminate ASAP by default.
--
-- To run the benchmark for an hour to improve accuracy,
-- run
--   stack bench --benchmark-arguments '-G -L30 -oout.html --csv summary.csv 2> fit.csv
myDefaultConfig = defaultConfig
  { timeLimit = 0.1
  }

sampleSizes :: Bool -> [Int]
sampleSizes False = [1024,2944..16384]
sampleSizes True  = [4096,8192..32768] -- 8 data points

randomGens :: Bool -> [StdGen]
randomGens False = [mkStdGen 0]
randomGens True  = splitManyTimes 32 (mkStdGen 0)
  where
    splitManyTimes 1 g = [g]
    splitManyTimes n g = let (g1, g2) = split g in g1 : splitManyTimes (n - 1) g2

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  wat <- execParser (describe myDefaultConfig)
  let longRunning = forceGCUnset wat
  let ns = sampleSizes longRunning
  let gs = randomGens  longRunning

  putStrLn ""
  putStrLn $ csvHeader ns
  putStr   $ unlines $ runAllKLDiv   gs ns
  --putStr   $ unlines $ runAllKSTests gs ns
  putStrLn ""

  --runMode (resetForceGC wat) $ runAllWeightedBayes ns bayesAlgs

----------------------
-- LIST OF SAMPLERS --
----------------------

type SampleFunction a = (forall m. MonadDist m => m a) -> StdGen -> a

samplers :: [(String, SampleFunction a)]
samplers =
  [ ("random-fu sampler"  , randomFuSampler)
  --, ("monad-bayes sampler", sample)
  ]

randomFuSampler :: forall a. SampleFunction a
randomFuSampler model gen = fst $ sampleState (model :: RVar a) gen

------------------------
-- LISTS OF ALGORITHMS --
------------------------

type BayesAlg a b = forall m. MonadDist m => String -> BayesM a -> Int -> m [b]

-- | Algorithms to benchmark performance
bayesAlgs :: [(String, BayesAlg a a)]
bayesAlgs =
  [ ("mh by time", mhByTimeAlg  )
  , ("smc"       , smcAlg       )
  , ("pimh"      , pimhAlg      )
  --, ("importance", importanceAlg)
  ]


type MultiSampler a = forall m. MonadDist m => String -> BayesM a -> [Int] -> m [[a]]

-- | Algorithms to benchmark goodness of fit
multiSamplers :: [(String, MultiSampler a)]
multiSamplers =
  [
    ("mh by time", \x m -> collectPrefixes (mhByTimeAlg x m))
  , ("mhPrior",    \x m -> collectPrefixes (flip mhPrior  m))
  , ("smc",        \x m -> collectReruns   (smcAlg      x m))
  -- , ("pimh",       \x m -> collectPrefixes (pimhAlg     x m)) -- enable alone later, is slow.
  ]

runBayesAlg :: BayesAlg a b -> String -> BayesM a -> SampleFunction [b] -> Int -> Int -> [b]
runBayesAlg alg modelName model sampler randomSeed sampleSize = sampler (alg modelName model sampleSize) (mkStdGen randomSeed)

importanceAlg :: BayesAlg a a
importanceAlg modelName model sampleSize =
  fmap (map (uncurry $ flip seq)) $ sequence $ replicate sampleSize (importance model)

mhByTimeAlg :: BayesAlg a a
mhByTimeAlg modelName model sampleSize = mh' ByTime.empty sampleSize model

-- use sample size as # particles
smcAlg :: BayesAlg a a
smcAlg = smcTemplate id $ \observations particles sampleSize model ->
  let
    particles = sampleSize
    smcIteration = runEmpiricalT $ smc observations particles model
  in
    fmap (map (uncurry $ flip seq)) smcIteration

pimhAlg :: BayesAlg a a
pimhAlg = smcTemplate (min 64) pimh

smcTemplate ::
  (Int -> Int) -> -- compute number of particles from sample size
  (forall m. MonadDist m => Int -> Int -> Int -> ParticleT (EmpiricalT m) a -> m [a]) -> BayesAlg a a
smcTemplate getParticles alg modelName model sampleSize =
  let
    observations = case lookup modelName modelObs of
                     Just obs -> obs
                     Nothing  -> error $ "Model not found in `modelObs`: " ++ modelName
    particles = getParticles sampleSize
  in
    alg observations particles sampleSize model

-- | Map model names to the number of observations
-- to have particles of equal weight in SMC.
--
-- The values here are checked at test/Spec.hs via
-- test/TestSMCObservations.hs.
modelObs :: [(String, Int)]
modelObs =
  [ ("Gamma.model"    , 5)
  , ("Gamma.exact"    , 0)
  , ("Dice.dice"      , 0)
  , ("Dice.dice_hard" , 1)
  , ("Dice.dice_soft" , 1)
  , ("BetaBin.latent" , 0)
  , ("BetaBin.urn"    , 0)
  , ("HMM.hmm"        , 16)
  , ("DPmixture.dpMem", 10)
  , ("DPmixture.dpMemClusters", 10)
  ]

---------------------
-- LISTS OF MODELS --
---------------------

type BayesM a = forall m. MonadBayes m => m a
type DistM  a = forall m. MonadDist  m => m a
type DistMs a = forall m. MonadDist  m => [m a]

-- Models measure fit by 2-sample Kolmogorov-Smirnov test
ksDouble :: [(String, BayesM Double, String, DistM Double)]
ksDouble =
  [ ("Gamma.model", Gamma.model, "Gamma.exact", Gamma.exact)
  , ("Gamma.exact", Gamma.exact, "Gamma.exact", Gamma.exact)
  ]

-- Models to measure fit by KL divergence, grouped by type.
-- Beware: Reference model must not contain any continuous
-- elementary random primitive.

klInt :: [(String, BayesM Int, String, BayesM Int)]
klInt =
  [ ("DPmixture.dpMemClusters", DPmixture.dpMemClusters, "posteriorClustersDist", DPmixture.posteriorClustersDist) ]
{-
  [ ("Dice.dice"     , Dice.dice 5   , "Dice.dice"     , Dice.dice 5   )
  , ("Dice.dice_hard", Dice.dice_hard, "Dice.dice_hard", Dice.dice_hard)
  , ("Dice.dice_soft", Dice.dice_soft, "Dice.dice_soft", Dice.dice_soft)
  ]
-}

{- HMM.exactMarginals isn't working
klInts :: [(String, BayesM [Int], String, DistMs Int)]
klInts =
  [ ("HMM.hmm", HMM.hmm, "HMM.exactMarginals", HMM.exactMarginals) ]
-}

klBools :: [(String, BayesM [Bool], String, BayesM [Bool])]
klBools =
  [ ("BetaBin.latent", BetaBin.latent 5, "BetaBin.urn", BetaBin.urn 5)
  , ("BetaBin.urn"   , BetaBin.urn 5   , "BetaBin.urn", BetaBin.urn 5)
  ]

-- Models too big to test by enumerating
bayesInts :: [(String, BayesM [Int], (), ())]
bayesInts =
  [ ("HMM.hmm", HMM.hmm, (), ())
  , ("DPmixture.dpMem", DPmixture.dpMem, (), ())
  ]

----------------------------
-- PERFORMANCE BENCHMARKS --
----------------------------

-- | Execute each combination of sample size, algorithm and model for one result type.
runWeightedBayes :: NFData a => [Int] -> [(String, BayesAlg a a)] -> [(String, BayesM a, x, y)] -> [Benchmark]
runWeightedBayes sampleSizes algorithms models =
  [ bgroup modelName
    [ bgroup algName
      [ bgroup samplerName
        [ bench (show n) $ nf (runBayesAlg alg modelName model sampler 0) n
        | n <- sampleSizes
        ]
      | (samplerName, sampler) <- samplers
      ]
    | (algName, alg) <- algorithms
    ]
  | (modelName, model, _, _) <- models
  ]

-- | Execute each combination of sample size and algorithm on all feasible models
runAllWeightedBayes :: [Int] -> (forall a. [(String, BayesAlg a a)]) -> [Benchmark]
runAllWeightedBayes sampleSizes algorithms =
  runWeightedBayes sampleSizes algorithms ksDouble ++
  runWeightedBayes sampleSizes algorithms klInt    ++
  runWeightedBayes sampleSizes algorithms klBools  ++
  runWeightedBayes sampleSizes algorithms bayesInts

---------------------------
-- GOODNESS-OF-FIT TESTS --
---------------------------

-- Collection of samples for fitting

-- | Collect samples of each size by re-running the sampler.
collectReruns :: (MonadDist m) => (Int -> m [a]) -> [Int] -> m [[a]]
collectReruns sampler sampleSizes = sequence $ map sampler sampleSizes

-- | Collect samples by taking prefixes of the biggest sample.
-- Justified for MCMC algorithms.
collectPrefixes :: (MonadDist m) => (Int -> m [a]) -> [Int] -> m [[a]]
collectPrefixes sampler sampleSizes = do
  samples <- sampler $ maximum sampleSizes
  return $ map (flip take samples) sampleSizes

-- | Given a series of samples, compute the KL-divergence of each.
klDivs :: (Ord a, Typeable a) => [[a]] -> Dist a -> [Double]
klDivs xss d = map (flip kullbackLeibnerTest d) xss

csvHeader :: [Int] -> String
csvHeader sampleSizes = "model,reference,algorithm,sampler,test," ++ intercalate "," (map show sampleSizes)

runKLDiv :: (NFData a, Ord a, Typeable a) =>
            [StdGen] -> [Int] -> [(String, BayesM a, String, BayesM a)] -> [String]
runKLDiv randomGens sampleSizes klModels =
  [ let
      collections = sampler (alg modelName model sampleSizes) randomGen
      klTestResults = map (flip kullbackLeibnerTest ref) collections
    in
      printf "%s,%s,%s,%s,%s," modelName refName algName samplerName "KL divergence" ++
        intercalate "," (map show klTestResults)
  | (modelName, model, refName, ref) <- klModels
  , (algName, alg)                   <- multiSamplers
  , (samplerName, sampler)           <- samplers
  , randomGen                        <- randomGens
  ]


runAllKLDiv :: [StdGen] -> [Int] -> [String]
runAllKLDiv randomGens sampleSizes =
  --runKLDiv randomGens sampleSizes klBools   ++
  runKLDiv randomGens sampleSizes klInt

{- pending fix of HMM.exactMarginals
  [ let
      collections = sampler (alg modelName model sampleSizes) randomGen  :: [[ [Int] ]] -- one per sample size
      getKL :: [[Int]] -> Double
      getKL samples = sum $ zipWith kullbackLeibnerTest (transpose samples) refs
      klTestResults = map getKL collections
    in
      printf "%s,%s,%s,%s,%s," modelName refName algName samplerName "KL divergence" ++
        intercalate "," (map show klTestResults)
  | (modelName, model, refName, refs) <- klInts
  , (algName, alg)                    <- multiSamplers
  , (samplerName, sampler)            <- samplers
  , randomGen                         <- randomGens
  ]
-}

-- | Plot supremum norm of the difference of empirical
-- density functions between two samples.
runKSTest :: (NFData a, Ord a, Typeable a) =>
             [StdGen] -> [Int] -> [(String, BayesM a, String, DistM a)] -> [String]
runKSTest randomGens sampleSizes ksModels =
  [ let
      (gen1, gen2) = split randomGen
      collections = sampler (alg modelName model sampleSizes) gen1
      refsamples  = sampler (sequence $ map (\n -> sequence $ replicate n ref) sampleSizes) gen2
      ksTestResults = zipWith supEDFdistance collections refsamples
    in
      printf "%s,%s,%s,%s,%s," modelName refName algName samplerName "KS test" ++
        intercalate "," (map show ksTestResults)
  | (modelName, model, refName, ref) <- ksModels
  , (algName, alg)                   <- multiSamplers
  , (samplerName, sampler)           <- samplers
  , randomGen                        <- randomGens
  ]

runAllKSTests :: [StdGen] -> [Int] -> [String]
runAllKSTests randomGens sampleSizes =
  runKSTest randomGens sampleSizes ksDouble

-- HACK: repurpose unused flag forceGC to decide whether to run for days

-- | Repurpose the unused -G flag for long-running benchmarks
forceGCUnset :: Mode -> Bool
forceGCUnset (Run cfg _ _) = not (forceGC cfg)
forceGCUnset _             = False

resetForceGC :: Mode -> Mode
resetForceGC (Run cfg x y) = Run (cfg { forceGC = True }) x y
resetForceGC mode          = mode
