{-# LANGUAGE
 DataKinds,
 FlexibleContexts,
 TypeFamilies
 #-}

module GaussianSum where

import Data.HList

import Base
import Dist

type J = HList ('[Double,Double])

jbind1 :: (HSplitAt (HSucc HZero) zs xs ys, zs ~ HAppendListR xs ys, HAppendList xs ys) => JDist (HList xs) a -> (a -> JDist (HList ys) b) -> JDist (HList zs) b
jbind1 = JBind

sumg :: JDist J Double
sumg =
    normal 0 1 `jbind1` \x ->
    normal 0 1 `jbind1` \y ->
    JReturn (x+y)

sum_result :: Double
sum_result = eval sumg $ hBuild 1 2

sum_density :: Prob
sum_density = density sumg $ hBuild 1 2

sum_marginal :: Dist Double
sum_marginal = marginal sumg

sum_joint :: Dist (HList '[Double,Double])
sum_joint = joint sumg

sum_proposal :: JDist J J
sum_proposal =
    normal 0 2 `jbind1` \x ->
    normal 0 2 `jbind1` \y ->
    JReturn (hBuild x y)

sum_proposed :: JDist J Double
sum_proposed = propose sum_proposal sumg
