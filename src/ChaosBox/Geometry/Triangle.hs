module ChaosBox.Geometry.Triangle
  ( Triangle(..)
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Math.Matrix           ( applyMatrix )
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon
import           Data.Foldable                  ( for_ )

data Triangle = Triangle
  { triangleA      :: V2 Double
  , triangleB      :: V2 Double
  , triangleC      :: V2 Double
  }

instance Affine Triangle where
  transform m t = t
    { triangleA = applyMatrix m (triangleA t)
    , triangleB = applyMatrix m (triangleB t)
    , triangleC = applyMatrix m (triangleC t)
    }

instance Draw Triangle where
  draw Triangle {..} = for_ (polygon [triangleA, triangleB, triangleC])
    draw
