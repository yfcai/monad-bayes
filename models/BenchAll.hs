import qualified BetaBin
import qualified Dice
import qualified DPmixture
import qualified Gamma
import qualified HMM

import Criterion.Main
import Criterion.Main.Options
import Criterion.Types

myDefaultConfig = defaultConfig
  { timeLimit = 0.1 -- run long-running benchmarks only once by default
  }



main :: IO ()
main = defaultMainWith myDefaultConfig [
  bgroup "fib" [ bench "x"  $ whnf fib 33
               ]
  ]

fib :: Integer -> Integer
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)
