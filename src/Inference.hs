{-# LANGUAGE
  FlexibleContexts,
  ScopedTypeVariables,
  Rank2Types,
  TupleSections
 #-}

module Inference where

import Control.Arrow (first,second)
import Data.Either
import Data.Number.LogFloat
import Data.Typeable
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Base
import Sampler
import Rejection
import Weighted
import Particle
import Empirical
import Dist
import Prior
import Trace

-- | Rejection sampling.
rejection :: MonadDist m => RejectionT m a -> m a
rejection d = do
  m <- runRejectionT d
  case m of Just x  -> return x
            Nothing -> rejection d

-- | Simple importance sampling from the prior.
importance :: MonadDist m => WeightedT m a -> m (a,LogFloat)
importance = runWeightedT

-- | Multiple importance samples with post-processing.
importance' :: (Ord a, Typeable a, MonadDist m) =>
               Int -> EmpiricalT m a -> m [(a,Double)]
importance' n d = fmap (enumerate . categorical) $ runEmpiricalT $ spawn n >> d

-- | Sequential Monte Carlo from the prior.
smc :: MonadDist m => Int -> ParticleT (EmpiricalT m) a -> EmpiricalT m a
smc n d = flatten $ run start where
    start = lift (spawn n) >> d
    step :: MonadDist m => ParticleT (EmpiricalT m) a -> ParticleT (EmpiricalT m) a
    step particles = mapMonad resample $ advance particles
    run :: MonadDist m => ParticleT (EmpiricalT m) a -> ParticleT (EmpiricalT m) a
    run particles = do
      finished <- lift $ lift $ Empirical.all id $ finished particles
      if finished then particles else run (step particles)

-- | `smc` with post-processing.
smc' :: (Ord a, Typeable a, MonadDist m) => Int ->
        ParticleT (EmpiricalT m) a -> m [(a,Double)]
smc' n d = fmap (enumerate . categorical) $ runEmpiricalT $ smc n d



-- | Metropolis-Hastings kernel. Generates a new value and the MH ratio.
newtype MHKernel m a = MHKernel {runMHKernel :: a -> m (a,LogFloat)}

mhKernel'  :: (RandomDB r, MonadDist m) => r -> UpdaterT r m a -> MHKernel m (a, r)
mhKernel' = const mhKernel

mhKernel :: (RandomDB r, MonadDist m) => UpdaterT r m a -> MHKernel m (a, r)
mhKernel program = MHKernel $ fmap fst . runWriterT . runMHKernel (mhKernelWithReuseLog program)

-- | Log of resampling of RandomDB at each Metropolis-Hastings step
data MHReuseLog = MHReuseLog
  { mhOldSize :: Int
  , mhNewSize :: Int
  , mhResampled :: Int
  }

instance Monoid MHReuseLog where
  mempty = MHReuseLog 0 0 0
  mappend (MHReuseLog o n r) (MHReuseLog o' n' r') = MHReuseLog (o + o') (n + n') (r + r')

mhKernelWithReuseLog' :: (RandomDB r, MonadDist m) =>
                         r -> UpdaterT r m a -> MHKernel (WriterT MHReuseLog m) (a, r)
mhKernelWithReuseLog' = const mhKernelWithReuseLog

mhKernelWithReuseLog :: (RandomDB r, MonadDist m) =>
                        UpdaterT r m a -> MHKernel (WriterT MHReuseLog m) (a, r)
mhKernelWithReuseLog program = MHKernel $ \(x, r) -> do
  r1 <- lift $ mutate r
  ((x', r'), leftover) <- lift $ runUpdaterT program r1
  tell $ MHReuseLog (size r) (size r') (size $ resampled r r')
  return ((x', r'), mhCorrectionFactor r r')

-- | Log of acceptance ratios of one Metropolis-Hastings step
data MHAcceptLog = MHAcceptLog
  { mhAccepted :: Int
  , mhTotalSteps :: Int
  , mhAcceptanceRatio :: Double
  }

instance Monoid MHAcceptLog where
  mempty = MHAcceptLog 0 0 0
  mappend (MHAcceptLog _ 0 _) r = r
  mappend (MHAcceptLog a n r) (MHAcceptLog a' n' r') =
    MHAcceptLog (a + a') (n + n') ((r * fromIntegral n + r' * fromIntegral n') / fromIntegral (n + n'))

-- | Execute Metropolis-Hastings algorithm, keep track of acceptance ratio
-- and other statistics.
mhWithAcceptLog :: forall m a. (MonadDist m) =>
                   Int ->  WeightedT m a -> MHKernel (WeightedT m) a -> WriterT MHAcceptLog m [a]
mhWithAcceptLog n init trans = evalStateT (start >>= chain n) 1 where
  start :: StateT LogFloat (WriterT MHAcceptLog m) a
  start = do
    (x, p) <- lift $ lift $ runWeightedT init
    put p
    return x

  chain :: Int -> a -> StateT LogFloat (WriterT MHAcceptLog m) [a]
  chain 0 _ = return []
  chain n x = do
    p <- get
    ((y,w), q) <- lift $ lift $ runWeightedT $ runMHKernel trans x
    let ratio = if p == 0 then 1 else min 1 (q * w / p)
    accept <- bernoulli ratio
    let next = if accept then y else x
    when accept (put q)
    lift $ tell (MHAcceptLog (if accept then 1 else 0) 1 (fromLogFloat ratio))
    rest <- chain (n-1) next
    return (x:rest)

-- | Metropolis-Hastings algorithm. The idea is that the kernel handles the
-- part of ratio that results from MonadDist effects and transition function,
-- while state carries the factor associated with MonadBayes effects.
mh :: MonadDist m => Int ->  WeightedT m a -> MHKernel (WeightedT m) a -> m [a]
mh n init trans = fmap fst $ runWriterT $ mhWithAcceptLog n init trans

-- | A wrapper of `mh` where the user supplies a program that instantiate to
-- both the initial run and the transition kernel.
mh' :: (RandomDB r, MonadDist m) => r -> Int -> (forall m'. (MonadBayes m') => m' a) -> m [a]
mh' r0 n program = fmap (map fst) $ mh n (runTraceT program) $ mhKernel' r0 program

mhWithStats :: forall r m a. (RandomDB r, MonadDist m) =>
               r -> Int -> WeightedT (WriterT MHReuseLog m) (a, r) ->
                           MHKernel (WeightedT (WriterT MHReuseLog m)) (a, r) ->
                           m ([a], MHAcceptLog, MHReuseLog)
mhWithStats r0 n init trans =
  fmap (\((results, accepts), reuses) -> (map fst results, accepts, reuses))
       (runWriterT $ runWriterT $ mhWithAcceptLog n init trans)

mhWithStats' :: forall r m a. (RandomDB r, MonadDist m) =>
                r -> Int -> (forall m'. (MonadBayes m') => m' a) -> m ([a], MHAcceptLog, MHReuseLog)
mhWithStats' r0 n program =
  mhWithStats r0 n (runTraceT program) $
    MHKernel $ pullOutWeightedT . runMHKernel (mhKernelWithReuseLog' r0 program)

-- | Metropolis-Hastings version that uses the prior as proposal distribution.
mhPrior :: MonadDist m => Int -> WeightedT m a -> m [a]
mhPrior n d = mh n d kernel where
    kernel = MHKernel $ const $ fmap (,1) d

-- | Particle Independent Metropolis Hastings. The first argument is the number
-- of particles in each SMC run, the second is the number of samples, equal to
-- the number of SMC runs.
pimh :: MonadDist m => Int -> Int -> ParticleT (EmpiricalT m) a -> m [a]
pimh np ns d = mhPrior np $ transform $ smc np d
