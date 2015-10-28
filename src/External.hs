
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    FlexibleContexts
    #-}

module External where

import Math.Gamma
import Numeric.Limits

import qualified Data.Random as Ext
import qualified Data.Random.Distribution.Bernoulli as Bern
import qualified Data.Random.Distribution.Categorical as Cat
import qualified Data.Random.Distribution.Exponential as Exp
import qualified Data.Random.Distribution.Gamma as Gamma
import qualified Data.Random.Distribution.Beta as Beta

instance Ext.PDF (Bern.Bernoulli Double) Bool where
        pdf (Bern.Bernoulli p) b = if b then p else 1-p

newtype Dirac a = Dirac a

instance Ext.Distribution Dirac a where
    rvarT (Dirac x) = return x

instance Eq a => Ext.PDF Dirac a where
    pdf (Dirac x) y = if x == y then 1 else 0


instance (Eq a) => Ext.PDF (Cat.Categorical Double) a where
    pdf d x = sum $ map fst $ filter ((== x) . snd) $ Cat.toList d

instance Ext.PDF Ext.Uniform Double where
    pdf (Ext.Uniform a b) x = if x >= a && x <= b then 1 / (b - a) else 0

instance Ext.PDF Exp.Exponential Double where
    logPdf (Exp.Exp l) x = if x < 0 then - infinity else (- l) * x + log l

instance Ext.PDF Gamma.Gamma Double where
    logPdf (Gamma.Gamma a b) x = if x < 0 then - infinity else
        a * log b - lnGamma a + (a-1) * log x - b * x 
