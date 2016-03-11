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
import Data.List (intercalate)
import Data.Number.LogFloat
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
import Sampler
import qualified Trace.ByDist as ByDist
import qualified Trace.ByTime as ByTime
import qualified Trace.ByType as ByType

-- | Terminate ASAP by default.
--
-- To run the benchmark for an hour to improve accuracy,
-- run `stack bench --benchmark-arguments '-L3600'`
myDefaultConfig = defaultConfig
  { timeLimit = 0.1
  }

sampleSizes :: Bool -> [Int]
sampleSizes False = [100, 200]
sampleSizes True  = [1024, 2048 .. 10240] -- 10 data points
                  --[1024, 5056 .. 65536] -- 16 data points

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  wat <- execParser (describe myDefaultConfig)
  let longRunning = forceGCUnset wat
  let ns = sampleSizes longRunning
  runMode (resetForceGC wat) $ runAllWeightedBayes ns bayesAlgs

  let (gen1, gen') = split $ mkStdGen 0
  let (gen2, gen3) = split gen'
  let randomGens = [gen1, gen2, gen3]

  putStrLn ""
  putStrLn $ csvHeader ns
  putStr   $ unlines $ runAllKLDiv   randomGens ns
  putStr   $ unlines $ runAllKSTests randomGens ns
  putStrLn ""

----------------------
-- LIST OF SAMPLERS --
----------------------

type SampleFunction a = (forall m. MonadDist m => m a) -> StdGen -> a

samplers :: [(String, SampleFunction a)]
samplers =
  [ ("monad-bayes sampler", sample)
  , ("random-fu sampler"  , randomFuSampler)
  ]

randomFuSampler :: forall a. SampleFunction a
randomFuSampler model gen = fst $ sampleState (model :: RVar a) gen

------------------------
-- LISTS OF ALGORITHMS --
------------------------

type BayesAlg a b = forall m. MonadDist m => BayesM a -> Int -> m [b]

-- | Algorithms to benchmark performance
bayesAlgs :: [(String, BayesAlg a a)]
bayesAlgs =
  [ ("importance", importanceAlg)
  --, ("smc"       , smcAlg       )
  , ("mh by time", mhByTimeAlg  )
  ]


type MultiSampler a = forall m. MonadDist m => BayesM a -> [Int] -> m [[a]]

-- | Algorithms to benchmark goodness of fit
multiSamplers :: [(String, MultiSampler a)]
multiSamplers =
  [ ("mh by time", collectPrefixes . mhByTimeAlg)
  ]

runBayesAlg :: BayesAlg a b -> BayesM a -> SampleFunction [b] -> Int -> Int -> [b]
runBayesAlg alg model sampler randomSeed sampleSize = sampler (alg model sampleSize) (mkStdGen randomSeed)

importanceAlg :: BayesAlg a a
importanceAlg model sampleSize =
  fmap (map (uncurry $ flip seq)) $ sequence $ replicate sampleSize (importance model)

smcAlg :: BayesAlg a a
smcAlg model sampleSize =
  fmap (map (uncurry $ flip seq)) $ runEmpiricalT $ smc sampleSize model

mhByTimeAlg :: BayesAlg a a
mhByTimeAlg model sampleSize = mh' ByTime.empty sampleSize model

---------------------
-- LISTS OF MODELS --
---------------------

type BayesM a = forall m. MonadBayes m => m a
type DistM  a = forall m. MonadDist  m => m a

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
  [ ("Dice.dice 5"   , Dice.dice 5   , "Dice.dice 5"   , Dice.dice 5   )
  , ("Dice.dice_hard", Dice.dice_hard, "Dice.dice_hard", Dice.dice_hard)
  , ("Dice.dice_soft", Dice.dice_soft, "Dice.dice_soft", Dice.dice_soft)
  ]

klBools :: [(String, BayesM [Bool], String, BayesM [Bool])]
klBools =
  [ ("BetaBin.latent 5", BetaBin.latent 5, "BetaBin.urn 5", BetaBin.urn 5)
  , ("BetaBin.urn 5"   , BetaBin.urn 5   , "BetaBin.urn 5", BetaBin.urn 5)
  ]

-- Beware: HMM is strict, DPmixture is lazy.
-- HMM is too big to test by KL divergence.
bayesInts :: [(String, BayesM [Int], (), ())]
bayesInts =
  [ ("HMM.hmm", HMM.hmm, (), ())
  --, ("DPmixture.dpMixture", DPmixture.dpMixture) -- not terminating in trace MH for now
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
        [ bench (show n) $ nf (runBayesAlg alg model sampler 0) n
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
      collections = sampler (alg model sampleSizes) randomGen
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
  runKLDiv randomGens sampleSizes klInt   ++
  runKLDiv randomGens sampleSizes klBools

-- | Plot supremum norm of the difference of empirical
-- density functions between two samples.
--
-- TODO: compute inverse of KS probability
-- for minimum alpha to accept the fit
runKSTest :: (NFData a, Ord a, Typeable a) =>
             [StdGen] -> [Int] -> [(String, BayesM a, String, DistM a)] -> [String]
runKSTest randomGens sampleSizes ksModels =
  [ let
      (gen1, gen2) = split randomGen
      collections = sampler (alg model sampleSizes) gen1
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
