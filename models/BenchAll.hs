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
import Data.Number.LogFloat
import Data.Random (RVar, sampleState)
import Options.Applicative (execParser)
import System.IO
import System.Random (StdGen, mkStdGen)

import Base
import Empirical
import Inference
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
    runWeightedBayesDist ns weightedBayes

-- LIST OF SAMPLERS

type SampleFunction a = (forall m. MonadDist m => m a) -> StdGen -> a

samplers :: [(String, SampleFunction a)]
samplers =
  [ ("monad-bayes sampler", sample)
  , ("random-fu sampler"  , randomFuSampler)
  ]

randomFuSampler :: forall a. SampleFunction a
randomFuSampler model gen = fst $ sampleState (model :: RVar a) gen

-- LISTS OF ALGORITHMS

type BayesAlg a b = forall m. MonadDist m => Bayes a -> Int -> m [b]

randomSeed = 0

runBayesAlg :: BayesAlg a b -> Bayes a -> SampleFunction [b] -> Int -> [b]
runBayesAlg alg model sampler sampleSize = sampler (alg model sampleSize) (mkStdGen randomSeed)

importanceAlg :: BayesAlg a a
importanceAlg model sampleSize =
  fmap (map (uncurry $ flip seq)) $ sequence $ replicate sampleSize (importance model)

smcAlg :: BayesAlg a a
smcAlg model sampleSize =
  fmap (map (uncurry $ flip seq)) $ runEmpiricalT $ smc sampleSize model

mhByTimeAlg :: BayesAlg a a
mhByTimeAlg model sampleSize = mh' ByTime.empty sampleSize model

weightedBayes :: [(String, BayesAlg a a)]
weightedBayes =
  [ ("importance", importanceAlg)
  --, ("smc"       , smcAlg       )
  , ("mh by time", mhByTimeAlg  )
  ]

-- LISTS OF MODELS

type Bayes a = forall m. MonadBayes m => m a
type Dist  a = forall m. MonadDist  m => m a

runWeightedBayes :: NFData a => [Int] -> [(String, BayesAlg a a)] -> [(String, Bayes a)] -> [Benchmark]
runWeightedBayes sampleSizes algorithms models =
  [ bgroup modelName
    [ bgroup (show n)
      [ bgroup algName
        [ bench samplerName $ nf (runBayesAlg alg model sampler) n
        | (samplerName, sampler) <- samplers
        ]
      | (algName, alg) <- algorithms
      ]
    | n <- sampleSizes
    ]
  | (modelName, model) <- models
  ]

runWeightedBayesDist :: [Int] -> (forall a. [(String, BayesAlg a a)]) -> [Benchmark]
runWeightedBayesDist sampleSizes algorithms =
  runWeightedBayes sampleSizes algorithms bayesDouble          ++
  runWeightedBayes sampleSizes algorithms (toBayes distDouble) ++
  runWeightedBayes sampleSizes algorithms bayesInt             ++
  runWeightedBayes sampleSizes algorithms (toBayes distInt)    ++
  runWeightedBayes sampleSizes algorithms (toBayes distBools)
  -- not including bayesInts for fear of nontermination

toBayes :: [(String, Dist a)] -> [(String, Bayes a)]
toBayes = map (\(s, m) -> (s, m))

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
