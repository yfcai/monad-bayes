{-# LANGUAGE
  FlexibleContexts,
  ScopedTypeVariables,
  Rank2Types,
  TupleSections,
  GeneralizedNewtypeDeriving
 #-}

module Control.Monad.Bayes.Inference where

import Control.Arrow (first,second)
import Data.Either
import Data.Number.LogFloat
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Rejection
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Particle
import Control.Monad.Bayes.Trace
import Control.Monad.Bayes.Empirical
import Control.Monad.Bayes.Dist
import Control.Monad.Bayes.Prior

-- | Rejection sampling.
rejection :: MonadDist m => Rejection m a -> m a
rejection d = do
  m <- runRejection d
  case m of Just x  -> return x
            Nothing -> rejection d

-- | Simple importance sampling from the prior.
importance :: MonadDist m => Weighted m a -> m (a,LogFloat)
importance = runWeighted

-- | Multiple importance samples with post-processing.
importance' :: (Ord a, MonadDist m) =>
               Int -> Population m a -> m [(a,Double)]
importance' n d = fmap (enumerate . categorical) $ runPopulation $ spawn n >> d

-- | Sequential Monte Carlo from the prior.
-- The first argument is the number of resampling points, the second is
-- the number of particles used.
-- If the first argument is smaller than the number of observations in the model,
-- the algorithm is still correct, but doesn't perform resampling after kth time.
smc :: MonadDist m => Int -> Int -> Particle (Population m) a -> Population m a
smc n k = smcWithResampler (resampleN n) n k

-- | `smc` with post-processing.
smc' :: (Ord a, MonadDist m) => Int -> Int ->
        Particle (Population m) a -> m [(a,Double)]
smc' n k d = fmap (enumerate . categorical) $ runPopulation $ smc n k d

-- | Asymptotically faster version of 'smc' that resamples using multinomial
-- instead of a sequence of categoricals.
smcFast :: MonadDist m => Int -> Int -> Particle (Population m) a -> Population m a
smcFast = smcWithResampler resample

composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

smcWithResampler :: MonadDist m =>
                    (forall x. Population m x -> Population m x) ->
                    Int -> Int -> Particle (Population m) a -> Population m a

smcWithResampler resampler n k =
  finish . composeCopies k (advance . hoistP resampler) . hoistP (spawn n >>)

smcrm :: forall m a. MonadDist m =>
         Int -> Int ->
         Particle (Trace (Population m)) a -> Population m a

smcrm n k = marginal . finish . composeCopies k step . init
  where
  init :: Particle (Trace (Population m)) a -> Particle (Trace (Population m)) a
  init = hoistP (hoistT (spawn n >>))

  step :: Particle (Trace (Population m)) a -> Particle (Trace (Population m)) a
  step = advance . hoistP (mhStep . hoistT resample)

-- | Metropolis-Hastings kernel. Generates a new value and the MH ratio.
newtype MHKernel m a = MHKernel {runMHKernel :: a -> m (a,LogFloat)}

-- | Metropolis-Hastings algorithm.
mh :: MonadDist m => Int ->  Weighted m a -> MHKernel (Weighted m) a -> m [a]
mh n init trans = evalStateT (start >>= chain n) 1 where
  -- start :: StateT LogFloat m a
  start = do
    (x, p) <- lift $ runWeighted init
    if p == 0 then
      start
    else
      put p >> return x

  --chain :: Int -> a -> StateT LogFloat m [a]
  chain 0 _ = return []
  chain n x = do
    p <- get
    ((y,w), q) <- lift $ runWeighted $ runMHKernel trans x
    accept <- bernoulli $ if p == 0 then 1 else min 1 (q * w / p)
    let next = if accept then y else x
    when accept (put q)
    rest <- chain (n-1) next
    return (x:rest)

-- | Trace MH. Each state of the Markov chain consists of a list
-- of continuations from the sampling of each primitive distribution
-- during an execution.
traceMH :: (MonadDist m) => Int -> Trace' m a -> m [a]
traceMH n (Trace' m) = m >>= init >>= loop n
  where
    init state | mhPosteriorWeight state >  0 = return state
    init state | mhPosteriorWeight state == 0 = m >>= init
    loop n state | n <= 0 = return []
    loop n state | n >  0 = do
      nextState <- mhKernel state
      otherAnswers <- loop (n - 1) nextState
      return (mhAnswer state : otherAnswers)

-- | Metropolis-Hastings version that uses the prior as proposal distribution.
mhPrior :: MonadDist m => Int -> Weighted m a -> m [a]
mhPrior n d = mh n d kernel where
    kernel = MHKernel $ const $ fmap (,1) d

-- | Particle Independent Metropolis Hastings. The first two arguments are
-- passed to SMC, the third is the number of samples, equal to
-- the number of SMC runs.
pimh :: MonadDist m => Int -> Int -> Int -> Particle (Population (Weighted m)) a -> m [a]
pimh k np ns d = mhPrior ns $ collapse $ smc k np d
