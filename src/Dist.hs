
{-# LANGUAGE
    GADTs,
    TupleSections,
    ScopedTypeVariables,
    KindSignatures,
    TypeOperators,
    DataKinds,
    FlexibleInstances
    #-}

module Dist where

import System.Random
import Control.Applicative (Applicative, pure, (<*>))
import Control.Arrow (first, second)
import Control.Monad (liftM, liftM2)

import qualified Data.Random as Ext
import qualified Data.Random.Distribution.Bernoulli as Bern

import Data.HList.HList
import Control.Monad.Indexed

import Base
import Explicit hiding (djoin)
import Sampler (external, StdSampler)
import External

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
    -- A posterior distribution composed of a prior and a likelihood.
    Conditional :: (a -> Prob) -> Dist a -> Dist a

instance Functor Dist where
    fmap  = liftM

instance Applicative Dist where
    pure  = return
    (<*>) = liftM2 ($)

instance Monad Dist where
    return = Return
    (>>=)  = Bind

instance Dirac Dist where
    dirac = return

instance Bernoulli Dist where
    bernoulli p = Primitive (bernoulli p :: StdSampler Bool)

instance UniformD Dist where
    uniformd = Primitive . (uniformd :: [a] -> StdSampler a)

instance Categorical Dist where
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



instance Conditional Dist where
    condition c d = Conditional c d

instance Sampler Dist where
    sampler = Primitive

instance Bayesian Dist where
    prior (Conditional c d) = do
        (x,s) <- prior d
        return (x, s * c x)
    --Prior is only extracted from the outer distribution.
    prior (Bind d f) = do
        (x,p) <- prior d
        y     <- f x
        return (y,p)
    -- Non-recursive cases are not conditional, so they just get score 1.
    prior d = fmap (,1) d

    prior' (Conditional c d) = prior' d
    prior' (Bind d f)        = prior' d >>= f
    prior' d = d


instance Sampleable Dist where
    sample g (Return x)     = x
    sample g (Primitive d)     = sample g d
    sample g (Bind d f)        = sample g1 $ f $ sample g2 d where
        (g1, g2) = split g
    sample g (Conditional c d) = error "Attempted to sample from a conditional distribution."


data JDist :: * -> * -> * -> * where
    JReturn :: a -> JDist x x a
    JBind :: JDist x y a -> (a -> JDist y z b) -> JDist  x z b
    JPrimitive :: (Ext.Distribution d a, Ext.PDF d a) =>
                  d a -> JDist (HList x) (HList (a ': x)) a
    JConditional :: (a -> Prob) -> JDist x y a -> JDist x y a

instance Functor (JDist x y) where
    fmap f d = d `JBind` (JReturn . f)

instance IxFunctor JDist where
    imap = fmap

instance IxPointed JDist where
    ireturn = JReturn

instance IxApplicative JDist where
    iap f d = f `JBind` \f -> d `JBind` (ireturn . f)

instance IxMonad JDist where
    ibind = flip JBind

instance Bernoulli (JDist (HList x) (HList (Bool ': x))) where
    bernoulli p = JPrimitive $ Bern.Bernoulli $ toDouble p

instance Normal (JDist (HList x) (HList (Double ': x))) where
    normal m s = JPrimitive $ Ext.Normal m s

instance Conditional (JDist x y) where
    condition = JConditional

instance Sampleable (JDist x y) where
    sample g (JReturn x) = x
    sample g (JBind d f) = sample g1 $ f $ sample g2 d where
                                (g1,g2) = split g
    sample g (JPrimitive d) = fst $ Ext.sampleState d g
    sample g (JConditional c d) = error "Attempted to sample from a conditional distribution."
 

marginal :: JDist x y a -> Dist a
marginal (JReturn x) = return x
marginal (JBind d f) = marginal d >>= (marginal . f)
marginal (JPrimitive d) = external d
marginal (JConditional c d) = Conditional c (marginal d)
