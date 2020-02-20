{-# LANGUAGE FlexibleContexts #-}
module ChaosBox.Random
  (
  -- * Distribution sampling
    uniform
  , unsafeUniform
  , weighted
  , unsafeWeighted
  , normal
  , stdNormal
  , gamma
  , triangular
  , bernoulli
  , sometimes
  -- * Collection operations
  , shuffle
  , sampleN
  -- * Re-Exports
  , MonadRandom.uniformMay
  , MonadRandom.weightedMay
  )
where

import           ChaosBox.Generate

import           Control.Monad.Random                (MonadRandom)
import qualified Control.Monad.Random                as MonadRandom
import           Data.Random                         (Distribution, Gamma,
                                                      Normal, StdUniform)
import qualified Data.Random                         as Random
import           Data.Random.Distribution.Bernoulli  (boolBernoulli)
import           Data.Random.Distribution.Triangular (floatingTriangular)
import           Data.RVar                           (sampleRVar)
import           Data.Semigroup.Foldable

-- | Sample a uniformly distributed element of a non-empty collection.
uniform :: (Foldable1 f, MonadRandom m) => f a -> m a
uniform = MonadRandom.uniform

-- | Sample a uniformly distributed element of a collection.
unsafeUniform :: (Foldable f, MonadRandom m) => f a -> m a
unsafeUniform = MonadRandom.uniform

-- | Sample a uniformly distributed element from a non-empty weighted collection.
weighted :: (Foldable1 f, MonadRandom m) => f (a, Rational) -> m a
weighted = MonadRandom.weighted

-- | Sample a uniformly distributed element from a weighted collection.
unsafeWeighted :: (Foldable f, MonadRandom m) => f (a, Rational) -> m a
unsafeWeighted = MonadRandom.weighted

-- | A normally distributed random variable.
normal
  :: (Distribution Normal a, Monad m)
  => a
  -- ^ Mean
  -> a
  -- ^ Standard Deviation
  -> GenerateT m a
normal a dev = sampleRVar (Random.normal a dev)

-- | A normally distributed random variable with center 0 & standard deviation 1.
stdNormal :: (Distribution Normal a, Monad m) => GenerateT m a
stdNormal = sampleRVar Random.stdNormal

-- | A gamma-distributed variable
gamma
  :: ( Floating a
     , Ord a
     , Distribution Normal a
     , Distribution StdUniform a
     , Monad m
     )
  => a
  -> a
  -> GenerateT m a
gamma a b = sampleRVar (Random.gamma a b)

-- | A triangular distributed random variable.
triangular
  :: (Floating a, Ord a, Distribution StdUniform a, Monad m)
  => a
  -- ^ Lower bound
  -> a
  -- ^ Midpoint
  -> a
  -- ^ Upper bound
  -> GenerateT m a
triangular lo mid hi = sampleRVar (floatingTriangular lo mid hi)

-- True or False with probability @P@.
bernoulli
  :: (Fractional a, Ord a, Distribution StdUniform a, Monad m)
  => a
  -- ^ @P@ (between 0 and 1)
  -> GenerateT m Bool
bernoulli = sampleRVar . boolBernoulli

-- | Alias for 'bernoulli'.
sometimes
  :: (Fractional a, Ord a, Distribution StdUniform a, Monad m)
  => a
  -> GenerateT m Bool
sometimes = bernoulli

-- | Shuffle a list.
shuffle :: Monad m => [a] -> GenerateT m [a]
shuffle = sampleRVar . Random.shuffle

-- | Sample @N@ elements of a list without replacement.
sampleN :: Monad m => Int -> [a] -> GenerateT m [a]
sampleN n xs = sampleRVar $ Random.shuffleNofM n (length xs) xs
