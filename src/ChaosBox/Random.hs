{-# LANGUAGE FlexibleContexts #-}
module ChaosBox.Random
  (
  -- * Distribution sampling
    uniform
  , unsafeUniform
  , uniformBounded
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
  -- * Higher-order functions
  , suchThat
  , unsafeSuchThat
  , uniformPointIn
  -- * Re-Exports
  , MonadRandom.uniformMay
  , MonadRandom.weightedMay
  )
where

import           ChaosBox.AABB
import           ChaosBox.Generate
import           ChaosBox.Geometry.Class
import           ChaosBox.Orphanage                  ()

import           Control.Monad.Random                (MonadRandom)
import qualified Control.Monad.Random                as MonadRandom
import           Data.Random                         (Distribution,
                                                      Distribution (..), Normal,
                                                      Normal (..), StdUniform)
import qualified Data.Random                         as Random
import           Data.Random.Distribution.Bernoulli  (boolBernoulli)
import           Data.Random.Distribution.Triangular (floatingTriangular)
import           Data.RVar                           (sampleRVar)
import           Data.Semigroup.Foldable
import           Linear.V2

-- | Sample a uniformly distributed element of a non-empty collection.
uniform :: (Foldable1 f, MonadRandom m) => f a -> m a
uniform = MonadRandom.uniform

-- | Sample a uniformly distributed element of a collection.
unsafeUniform :: (Foldable f, MonadRandom m) => f a -> m a
unsafeUniform = MonadRandom.uniform

-- | A uniformly distributed random variable of a 'Bounded' 'Enum'.
uniformBounded :: (Monad m, Enum a, Bounded a) => GenerateT m a
uniformBounded = MonadRandom.uniform [minBound .. maxBound]

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

-- | Generate a random variable satisfying a given predicate safely.
--
-- Attempts to generate a variable a maximum of @1,000@ times. If the
-- predicate is never satisfied, returns 'Nothing'.
--
suchThat :: Monad m => GenerateT m a -> (a -> Bool) -> GenerateT m (Maybe a)
suchThat gen predicate = go (1000 :: Int)
 where
  go 0 = pure Nothing
  go n = do
    a <- gen
    if predicate a then pure (Just a) else go (n - 1)

-- | Generate a random variable satisfying a given predicate.
--
-- Will 'error' after @1,000@ failed generations.
--
unsafeSuchThat :: Monad m => GenerateT m a -> (a -> Bool) -> GenerateT m a
unsafeSuchThat gen predicate = do
  ma <- gen `suchThat` predicate
  case ma of
    Nothing ->
      error
        "Error in 'unsafeSuchThat': Maximum generation attempts (1000) exceeded."
    Just x -> pure x

-- | Generate a uniformly distributed point within a bounded shape
uniformPointIn
  :: (Boundary a, HasAABB a, Monad m) => a -> GenerateT m (Maybe (V2 Double))
uniformPointIn a = genPointInAABB (aabb a) `suchThat` containsPoint a
 where
  genPointInAABB (AABB (V2 tx ty) w h) = do
    x <- MonadRandom.getRandomR (tx, tx + w)
    y <- MonadRandom.getRandomR (ty, ty + h)
    pure $ V2 x y
