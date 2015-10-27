
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
