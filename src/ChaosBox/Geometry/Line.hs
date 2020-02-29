module ChaosBox.Geometry.Line
  ( LineOf(..)
  , Line
  , line
  )
where

import           ChaosBox.Geometry.P2
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Path
import           ChaosBox.AABB
import           ChaosBox.Geometry.Class
import           Data.List.NonEmpty

data LineOf a = LineOf { lineStart :: a, lineEnd :: a}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Line = LineOf (P2)

line :: a -> a -> LineOf a
line = LineOf

instance HasV2 a => HasAABB (LineOf a) where
  aabb LineOf {..} = boundary $ lineStart :| [lineEnd]

instance HasV2 a => Affine (LineOf a) where
  transform = defaultTransform

instance HasV2 a => Draw (LineOf a) where
  draw LineOf {..} = draw $ PathOf (lineStart :| [lineEnd])
