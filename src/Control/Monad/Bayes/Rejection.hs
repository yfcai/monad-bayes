{-|
Module      : Control.Monad.Bayes.Rejection
Description : Rejection-based probability monad
Copyright   : (c) Adam Scibior, 2016
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  TypeFamilies
 #-}

module Control.Monad.Bayes.Rejection (
                  Rejection,
                  runRejection) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Class

-- | A probability monad that aborts when a condition is violated.
-- Only accepts factors in [0,1] range.
newtype Rejection m a = Rejection {toMaybeT :: (MaybeT m a)}
    deriving (Functor, Applicative, Monad, MonadTrans)

-- | Run the probabilistic computation and produce a result if all conditions
-- are satisfied.
runRejection :: Rejection m a -> m (Maybe a)
runRejection = runMaybeT . toMaybeT

type instance CustomReal (Rejection m) = CustomReal m

instance MonadDist m => MonadDist (Rejection m) where
  primitive = lift . primitive

instance MonadDist m => MonadBayes (Rejection m) where
  factor w | w > 1 = error $ "Rejection: factor " ++ show (realToFrac w) ++ " is not in [0,1] range."
  factor w = do
    accept <- bernoulli (fromLogDomain w)
    unless accept (fail "")
