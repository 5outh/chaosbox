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

import qualified Control.Monad.Random                as MonadRandom
import           Data.Random                         (Distribution, Gamma,
                                                      Normal, StdUniform)
import qualified Data.Random                         as Random
import           Data.Random.Distribution.Bernoulli  (boolBernoulli)
import           Data.Random.Distribution.Triangular (floatingTriangular)
import           Data.RVar                           (sampleRVar)
import           Data.Semigroup.Foldable

-- | Sample a uniformly distributed element of a non-empty collection.
uniform :: Foldable1 f => f a -> Generate a
uniform = MonadRandom.uniform

-- | Sample a uniformly distributed element of a collection.
unsafeUniform :: Foldable f => f a -> Generate a
unsafeUniform = MonadRandom.uniform

-- | Sample a uniformly distributed element from a non-empty weighted collection.
weighted :: Foldable1 f => f (a, Rational) -> Generate a
weighted = MonadRandom.weighted

-- | Sample a uniformly distributed element from a weighted collection.
unsafeWeighted :: Foldable f => f (a, Rational) -> Generate a
unsafeWeighted = MonadRandom.weighted

-- | A normally distributed random variable.
normal
  :: Distribution Normal a
  => a
  -- ^ Mean
  -> a
  -- ^ Standard Deviation
  -> Generate a
normal a dev = sampleRVar (Random.normal a dev)

-- | A normally distributed random variable with center 0 & standard deviation 1.
stdNormal :: Distribution Normal a => Generate a
stdNormal = sampleRVar Random.stdNormal

-- | A gamma-distributed variable
gamma
  :: (Floating a, Ord a, Distribution Normal a, Distribution StdUniform a)
  => a
  -> a
  -> Generate a
gamma a b = sampleRVar (Random.gamma a b)

-- | A triangular distributed random variable.
triangular
  :: (Floating a, Ord a, Distribution StdUniform a)
  => a
  -- ^ Lower bound
  -> a
  -- ^ Midpoint
  -> a
  -- ^ Upper bound
  -> Generate a
triangular lo mid hi = sampleRVar (floatingTriangular lo mid hi)

-- True or False with probability @P@.
bernoulli
  :: (Fractional a, Ord a, Distribution StdUniform a)
  => a
  -- ^ @P@ (between 0 and 1)
  -> Generate Bool
bernoulli = sampleRVar . boolBernoulli

-- | Alias for 'bernoulli'.
sometimes
  :: (Fractional a, Ord a, Distribution StdUniform a) => a -> Generate Bool
sometimes = bernoulli

-- | Shuffle a list.
shuffle :: [a] -> Generate [a]
shuffle = sampleRVar . Random.shuffle

-- | Sample @N@ elements of a list without replacement.
sampleN :: Int -> [a] -> Generate [a]
sampleN n xs = sampleRVar $ Random.shuffleNofM n (length xs) xs
