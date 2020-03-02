module ChaosBox.Geometry.Line
  ( LineOf(..)
  , Line
  , pattern Line
  )
where

import           ChaosBox.AABB
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Path
import           Data.List.NonEmpty

data LineOf a = LineOf { lineStart :: a, lineEnd :: a}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Line = LineOf P2

pattern Line :: P2 -> P2 -> Line
pattern Line s e = LineOf s e

instance HasP2 a => HasAABB (LineOf a) where
  aabb LineOf {..} = boundary $ lineStart :| [lineEnd]

instance HasP2 a => Affine (LineOf a) where
  transform = defaultTransform

instance HasP2 a => Draw (LineOf a) where
  draw LineOf {..} = draw $ PathOf (lineStart :| [lineEnd])
