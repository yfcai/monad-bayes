{-# LANGUAGE ImpredicativeTypes #-}

-- Import all models under maintenance.
-- Models not imported here will not be compiled
-- when invoking `stack bench`.
import qualified BetaBin
import qualified Dice
import qualified DPmixture
import qualified Gamma
import qualified HMM

import Criterion.Main
import Criterion.Main.Options
import Criterion.Types
import Data.Number.LogFloat
import Options.Applicative (execParser)
import System.IO
import System.Random (mkStdGen)

import Base
import Empirical
import Inference
import Sampler

-- | Terminate ASAP by default.
--
-- To run the benchmark for an hour to improve accuracy,
-- run `stack bench --benchmark-arguments '-L3600'`
myDefaultConfig = defaultConfig
  { timeLimit = 0.1
  }

sampleSizes :: Bool -> [Int]
sampleSizes False = [1024]
sampleSizes True  = [1024, 2048 .. 10240] -- 10 data points
                  --[1024, 5056 .. 65536] -- 16 data points

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- DELETE THIS
  wat <- execParser (describe myDefaultConfig)
  let longRunning = forceGCUnset wat
  let ns = sampleSizes longRunning
  runMode (resetForceGC wat) $
    [ runWeightedBayesDist ns "importance" importanceAlg
    , runWeightedBayesDist ns "smc" smcAlg
    ]

-- LISTS OF ALGORITHMS

type BayesAlg a b = Bayes a -> Int -> Sampler [b]

randomSeed = 0

runBayesAlg :: BayesAlg a b -> Bayes a -> Int -> [b]
runBayesAlg alg model sampleSize = sample (alg model sampleSize) (mkStdGen randomSeed)

importanceAlg :: BayesAlg a (a, LogFloat)
importanceAlg model sampleSize = sequence $ replicate sampleSize (importance model)

smcAlg :: BayesAlg a (a, LogFloat)
smcAlg model sampleSize = runEmpiricalT $ smc sampleSize model

-- LISTS OF MODELS

type Bayes a = forall m. MonadBayes m => m a
type Dist  a = forall m. MonadDist  m => m a

-- The seemingly identical lines differ only in the typeclass instances supplied to `nf`
runWeightedBayesDist :: [Int] -> String -> (forall a. BayesAlg a (a, LogFloat)) -> Benchmark
runWeightedBayesDist sampleSizes algName alg =
  bgroup algName $
    [ bgroup modelName [ bench (show n) $ nf (map fst) (runBayesAlg alg model n) | n <- sampleSizes ]
    | (modelName, model) <- bayesDouble
    ]
    ++
    [ bgroup modelName [ bench (show n) $ nf (map fst) (runBayesAlg alg model n) | n <- sampleSizes ]
    | (modelName, model) <- bayesInt
    ]
    ++
    [ bgroup modelName [ bench (show n) $ nf (map fst) (runBayesAlg alg model n) | n <- sampleSizes ]
    | (modelName, model) <- distDouble
    ]
    ++
    [ bgroup modelName [ bench (show n) $ nf (map fst) (runBayesAlg alg model n) | n <- sampleSizes ]
    | (modelName, model) <- distBools
    ]
    ++
    [ bgroup modelName [ bench (show n) $ nf (map fst) (runBayesAlg alg model n) | n <- sampleSizes ]
    | (modelName, model) <- distInt
    ]
    -- not including bayesInts for fear of nontermination

bayesDouble :: [(String, Bayes Double)]
bayesDouble =
  [ ("Gamma.model", Gamma.model)
  ]

bayesInt :: [(String, Bayes Int)]
bayesInt =
  [ ("Dice.dice_hard", Dice.dice_hard)
  , ("Dice.dice_soft", Dice.dice_soft)
  ]

-- termination behavior is not clear
bayesInts :: [(String, Bayes [Int])]
bayesInts =
  [ ("DPmixture.dpMixture", DPmixture.dpMixture)
  , ("HMM.hmm", HMM.hmm)
  ]

distDouble :: [(String, Dist Double)]
distDouble =
  [ ("Gamma.exact", Gamma.exact)
  ]

distBools :: [(String, Dist [Bool])]
distBools =
  [ ("BetaBin.latent 10", BetaBin.latent 10)
  , ("BetaBin.urn 10", BetaBin.urn 10)
  ]

distInt :: [(String, Dist Int)]
distInt =
  [ ("Dice.dice 10", Dice.dice 10)
  ]


-- HACK: repurpose unused flag forceGC to decide whether to run for days

-- | Repurpose the unused -G flag for long-running benchmarks
forceGCUnset :: Mode -> Bool
forceGCUnset (Run cfg _ _) = not (forceGC cfg)
forceGCUnset _             = False

resetForceGC :: Mode -> Mode
resetForceGC (Run cfg x y) = Run (cfg { forceGC = True }) x y
resetForceGC mode          = mode
