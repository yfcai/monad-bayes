
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    FlexibleContexts
    #-}

module External where

import qualified Data.Random as Ext
import qualified Data.Random.Distribution.Bernoulli as Bern

instance Ext.PDF (Bern.Bernoulli Double) Bool where
        pdf (Bern.Bernoulli p) b = if b then p else 1-p

newtype Dirac a = Dirac a

instance Ext.Distribution Dirac a where
    rvarT (Dirac x) = return x

instance Eq a => Ext.PDF Dirac a where
    pdf (Dirac x) y = if x == y then 1 else 0
