{-# LANGUAGE
  TupleSections,
  RebindableSyntax
 #-}

module SMC where

import Prelude hiding (sequence, mapM, (>>=))

import Base hiding (score)
import Dist
import Importance
import Explicit (Explicit(Explicit))
import qualified Explicit

import Control.Monad.Parameterized (returnM, (>>=))
import Control.Arrow (first,second)
import Control.Monad (join)
import Control.Applicative (liftA)
import Data.Traversable (Traversable, traverse, sequenceA, sequence, mapM)
import Data.Foldable (Foldable, foldMap)

-- | The Sequential Monte Carlo algorithm.
-- Introduces artificial conditioning by the pseudo-marginal likelihood
-- to correct the bias.
smc  :: Int -> CDist a -> CDist (Samples a)
smc n (Conditional c d) = updated >>= resample where
  updated = fmap normalize $
            condition (sum . map snd) $ do
              ps    <- smc n d
              let qs = map (\(x,w) -> (x, c x * w)) ps
              returnM qs
smc n (CBind d f) = do
  ps <- smc n d
  let (xs,ws) = unzip ps
  ys <- mapM f xs
  returnM (zip ys ws)
smc n (Pure d) = Pure $ sequence $ replicate n $ fmap (,1) d

smc' :: Int -> CDist a -> CDist a
smc' n d = smc n d >>= categorical

-- | A variant of `smc` that discards artificial conditioning.
smcStandard :: Int -> CDist a -> Dist (Samples a)
smcStandard n = prior' . smc n

smcStandard' :: Int -> CDist a -> Dist a
smcStandard' n = prior' . smc' n

-- | Runs `smc` multiple times and aggregates results using
-- the pseudo-marginal likelihood.
smcMultiple :: Int -> Int -> CDist a -> Dist (Samples a)
smcMultiple k n = fmap flatten . importance k . smc n

smcMultiple' :: Int -> Int -> CDist a -> Dist a
smcMultiple' k n = importance' k . smc' n

-- | The Particle Cascade algorithm, produces an infinite list
-- of weighted samples.
cascade  :: CDist a -> Dist (Samples a)
cascade (Conditional c d) = do
  ps    <- cascade d
  let qs = map (\(x,w) -> (x, c x * w)) ps
  resamplePC qs
cascade (Bind d f) = do
  ps <- cascade d
  let (xs,ws) = unzip ps
  ys <- mapM f xs
  return (zip ys ws)
cascade (Pure d) = sequence $ repeat $ fmap (,1) d

cascade' :: Int -> CDist a -> Dist a
cascade' n d = cascade d >>= categorical . take n

resamplePC :: Samples a -> Dist (Samples a)
resamplePC ps =
    iterate 0 0 ps where
      iterate :: Int -> Prob -> Samples a -> Dist (Samples a)
      iterate n mean ((x,w):ps) =
        let
          k = fromIntegral n
          mean' = (k / (k + 1)) * mean + (1 / (k + 1)) * w
          r = w / mean'
          flr = floor r :: Int
          probLow = fromIntegral flr
          clr = ceiling r :: Int
          probHigh = fromIntegral clr
          spawn x w =
            if r < 1 then
              choice r (return [(x,mean')]) (return [])
            else
              choice (r - probLow)
              (return $ replicate flr (x, w / probLow))
              (return $ replicate clr (x, w / probHigh))
        in
         do
           children <- spawn x w
           rest <- iterate (n+1) mean' ps
           (return :: a -> Dist a) $ children ++ rest

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y
