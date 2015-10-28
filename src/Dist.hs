
{-# LANGUAGE
    GADTs,
    TupleSections,
    ScopedTypeVariables,
    KindSignatures,
    TypeOperators,
    DataKinds,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances
    #-}

module Dist where

import System.Random
import Control.Applicative (Applicative, pure, (<*>))
import Control.Arrow (first, second)
import Control.Monad (liftM, liftM2)
import Data.Proxy

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


data JDist :: * -> * -> * where
    JReturn :: a -> JDist (HList '[])  a
    JBind :: (HSplitAt n zs xs ys) => JDist (HList xs) a ->
             (a -> JDist (HList ys) b) -> JDist (HList zs) b
    JPrimitive :: (Ext.Distribution d a, Ext.PDF d a) =>
                  d a -> JDist (HList (a ': '[])) a
    JConditional :: (a -> Prob) -> JDist x a -> JDist x a

instance HSplitAt n xs xs '[] => Functor (JDist (HList xs)) where
    fmap = jmap

jmap :: HSplitAt n xs xs '[] =>
        (a -> b) -> JDist (HList xs) a -> JDist (HList xs) b
jmap f d = d `JBind` (JReturn . f)

instance Bernoulli (JDist (HList '[Bool])) where
    bernoulli p = JPrimitive $ Bern.Bernoulli $ toDouble p

instance Normal (JDist (HList '[Double])) where
    normal m s = JPrimitive $ Ext.Normal m s

instance Conditional (JDist x) where
    condition = JConditional

instance Sampleable (JDist x) where
    sample g (JReturn x) = x
    sample g (JBind d f) = sample g1 $ f $ sample g2 d where
                              (g1,g2) = split g
    sample g (JPrimitive d) = fst $ Ext.sampleState d g
    sample g (JConditional c d) = error "Attempted to sample from a conditional distribution."

eval :: JDist (HList x) a -> HList x -> a
eval (JReturn a) _ = a
eval (JBind d f) xs = f (eval d xs1) `eval` xs2
    where (xs1,xs2) = hSplitAt Proxy xs 
eval (JPrimitive d) xs = hHead xs
eval (JConditional c d) xs = eval d xs


density :: JDist (HList x) a -> HList x -> Prob
density (JReturn _) _  = 1
density (JBind d f) xs = density d xs1 * density (f x) xs2 where
    (xs1,xs2) = hSplitAt Proxy xs
    x = eval d xs1
density (JPrimitive d) xs = prob $ Ext.pdf d (hHead xs)
density (JConditional c d) xs = c (eval d xs) * density d xs

marginal :: JDist x a -> Dist a
marginal (JReturn x) = return x
marginal (JBind d f) = marginal d >>= (marginal . f)
marginal (JPrimitive d) = external d
marginal (JConditional c d) = Conditional c (marginal d)

propose :: (HSplitAt n xs xs '[]) => JDist (HList xs) (HList xs) ->
           JDist (HList xs) a -> JDist (HList xs) a
propose new old = fmap (eval old) $ condition c new where
    c x = density old x / density new x

