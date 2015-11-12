{-# LANGUAGE
  GADTs,
  TupleSections,
  ScopedTypeVariables,
  MultiParamTypeClasses,
  FlexibleInstances,
  RebindableSyntax
 #-}

module Dist where

import Prelude hiding ((>>=))

import System.Random
import Control.Applicative (Applicative, pure, (<*>))
import Control.Arrow (first, second)
import qualified Control.Monad
import Control.Monad.Parameterized (Return, returnM, Bind, (>>=), liftM, liftM2)

import Base
import Explicit hiding (djoin)
import Sampler (external, StdSampler)

-- | A symbolic representation of a probabilistic program which basically remembers all applications of 'return' and '>>='.
-- Formally a free model for a probability monad.
-- Additional constructors are Primitive and Conditional.
data Dist a where
    -- One element degenerate distribution.
    Return      :: a -> Dist a
    -- Application of a function to a random variable.
    Bind        :: Dist b -> (b -> Dist a) -> Dist a
    -- A primitive distribution that can be sampled from.
    Primitive   :: (Sampleable d) => d a -> Dist a

instance Functor Dist where
    fmap f x = x `Bind` (Return . f)

instance Applicative Dist where
    pure    = Return
    f <*> x = f `Bind` (`fmap` x) 

instance Monad Dist where
    return = Return
    (>>=)  = Bind


instance Return Dist where
  returnM = Return

instance Bind Dist Dist Dist where
  (>>=) = Bind


instance Dirac a Dist where
    dirac = return

instance Bernoulli Dist where
    bernoulli p = Primitive (bernoulli p :: StdSampler Bool)

instance UniformD a Dist where
    uniformd = Primitive . (uniformd :: [a] -> StdSampler a)

instance Categorical a Dist where
    categorical = Primitive . (categorical :: [(a,Prob)] -> StdSampler a)

instance Normal Dist where
    normal m s     = Primitive (normal m s :: StdSampler Double)

instance UniformC Dist where
    uniformc a b = Primitive (uniformc a b :: StdSampler Double)

instance Exponential Dist where
    exponential l = Primitive (exponential l :: StdSampler Double)

instance Gamma Dist where
    gamma a b = Primitive (gamma a b :: StdSampler Double)

instance Beta Dist where
    beta a b = Primitive (beta a b :: StdSampler Double)



instance Sampler Dist where
    sampler = Primitive

instance Sampleable Dist where
    sample g (Return x)     = x
    sample g (Primitive d)  = sample g d
    sample g (Bind d f)     = sample g1 $ f $ sample g2 d where
        (g1, g2) = split g



data CDist a where
    -- Non-conditional distribution.
    Pure        :: Dist a -> CDist a
    -- Application of a function to a random variable.
    CBind        :: CDist b -> (b -> Dist a) -> CDist a
    -- A primitive distribution that can be sampled from.
    Conditional :: (a -> Prob) -> CDist a -> CDist a

instance Functor CDist where
  fmap f x = x `CBind` (returnM . f)

instance Return CDist where
  returnM = Pure . returnM

instance Bind CDist Dist CDist where
  (>>=) = CBind


instance Dirac a CDist where
    dirac = Pure . dirac

instance Bernoulli CDist where
    bernoulli = Pure . bernoulli

instance UniformD a CDist where
    uniformd = Pure . uniformd

instance Categorical a CDist where
    categorical = Pure . categorical

instance Normal CDist where
    normal m s = Pure $ normal m s

instance UniformC CDist where
    uniformc a b = Pure $ uniformc a b

instance Exponential CDist where
    exponential = Pure . exponential

instance Gamma CDist where
    gamma a b = Pure $ gamma a b

instance Beta CDist where
    beta a b = Pure $ beta a b


instance Conditional CDist where
  condition = Conditional

instance Sampler CDist where
  sampler = Pure . sampler

--instance Bayesian CDist Dist where
prior :: CDist a -> Dist (a,Prob)
prior (Conditional c d) = do
  (x,s) <- prior d
  (returnM :: a -> Dist a) (x, s * c x)
prior (CBind d f) = do
  (x,p) <- prior d
  y     <- f x
  (returnM :: a -> Dist a) (y,p)
prior (Pure d) = fmap (,1) d

prior' :: CDist a -> Dist a
prior' (Conditional c d) = prior' d
prior' (CBind d f)       = prior' d >>= f
prior' (Pure d)          = d



