module ChaosBox.Geometry.Line
  ( LineOf(..)
  , Line
  , line
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Path
import qualified ChaosBox.Geometry.Rect as Rect
import           ChaosBox.HasAABB
import           ChaosBox.HasV2
import           Data.List.NonEmpty

data LineOf a = LineOf { lineStart :: a, lineEnd :: a}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Line = LineOf (V2 Double)

line :: a -> a -> LineOf a
line = LineOf

instance HasV2 a => HasAABB (LineOf a) where
  aabb LineOf {..} = Rect.bounds [lineStart, lineEnd]

instance HasV2 a => Affine (LineOf a) where
  transform = defaultTransform

instance HasV2 a => Draw (LineOf a) where
  draw LineOf {..} = draw $ PathOf (lineStart :| [lineEnd])
