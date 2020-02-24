{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module ChaosBox.Geometry.Path
  ( PathOf(..)
  , Path
  , path
  , pathOf
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Math.Matrix     (applyMatrix)
import           Control.Lens             (Lens', lens, (%~), (^.))
import           Data.Foldable            (for_)
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Graphics.Rendering.Cairo hiding (Path)

class HasV2 a where
  _V2 :: Lens' a (V2 Double)

instance a ~ Double => HasV2 (V2 a) where
  _V2 = lens id const

newtype PathOf a = PathOf { getPathOf :: NonEmpty a}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

-- | An open path
type Path = PathOf (V2 Double)

instance HasV2 a => Affine (PathOf a) where
  transform m = PathOf . fmap (_V2._xy %~ applyMatrix m) . getPathOf

instance HasV2 a => Draw (PathOf a) where
  draw (PathOf (start :| rest)) = do
    newPath
    moveTo (start ^. _V2._x) (start ^. _V2._y)
    for_ (map (^._V2._xy) rest) (\(V2 x y) -> lineTo x y)

path :: [V2 Double] -> Maybe Path
path = pathOf

pathOf :: [a] -> Maybe (PathOf a)
pathOf xs = PathOf <$> NE.nonEmpty xs
