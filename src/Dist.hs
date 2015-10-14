
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RebindableSyntax #-}

module Dist where

import Prelude hiding ((>>=))

import System.Random
import Data.Random.Distribution.Beta (Beta(Beta))
import Data.Random.Distribution.Exponential (Exponential(Exp))
import qualified Data.Random as Ext
import Control.Applicative (Applicative, pure, (<*>))
import Control.Arrow (first, second)
import qualified Control.Monad
import Control.Monad.Parameterized (Return, returnM, Bind, (>>=), liftM, liftM2)

import Base
import Explicit hiding (djoin)
import Sampler (external)

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

instance DiscreteDist Dist where
    categorical = Primitive . (categorical :: [(a,Prob)] -> Explicit a)

instance ContinuousDist Dist where
    normal m s     = external $ Ext.Normal m s
    gamma  k t     = external $ Ext.Gamma  k t
    beta   a b     = external $ Beta       a b
    exponential  l = external $ Exp        l

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



instance DiscreteDist CDist where
  categorical = Pure . categorical

instance ContinuousDist CDist where
  normal m s     = Pure $ normal m s
  gamma  k t     = Pure $ gamma k t
  beta   a b     = Pure $ beta a b
  exponential  l = Pure $ exponential l

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



