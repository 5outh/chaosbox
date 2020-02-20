{-# LANGUAGE FlexibleContexts #-}
module ChaosBox.Random
  ( normal
  , stdNormal
  , gamma
  , bernoulli
  , sometimes
  )
where

import           ChaosBox.Generate

import           Data.Random                         (Distribution, Gamma,
                                                      Normal, StdUniform)
import qualified Data.Random                         as Random
import           Data.Random.Distribution.Bernoulli  (boolBernoulli)
import           Data.Random.Distribution.Triangular (floatingTriangular)
import           Data.RVar                           (sampleRVar)

normal :: Distribution Normal a => a -> a -> Generate a
normal a dev = sampleRVar (Random.normal a dev)

stdNormal :: Distribution Normal a => Generate a
stdNormal = sampleRVar Random.stdNormal

gamma
  :: (Floating a, Ord a, Distribution Normal a, Distribution StdUniform a)
  => a
  -> a
  -> Generate a
gamma a b = sampleRVar (Random.gamma a b)

triangular
  :: (Floating a, Ord a, Distribution StdUniform a) => a -> a -> a -> Generate a
triangular lo mid hi = sampleRVar (floatingTriangular lo mid hi)

bernoulli
  :: (Fractional a, Ord a, Distribution StdUniform a) => a -> Generate Bool
bernoulli = sampleRVar . boolBernoulli

sometimes
  :: (Fractional a, Ord a, Distribution StdUniform a) => a -> Generate Bool
sometimes = bernoulli
