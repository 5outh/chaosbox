module ChaosBox.Geometry.Triangle
  ( Triangle(..)
  , triangle
  , bakeTriangle
  )
where

import           ChaosBox.Prelude

import           Control.Lens                   ( set )
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Polygon
import           Data.Foldable                  ( for_ )

data Triangle = Triangle
  { triangleA      :: V2 Double
  , triangleB      :: V2 Double
  , triangleC      :: V2 Double
  , triangleMatrix :: M33 Double
  }

instance Affine Triangle where
  matrixLens wrap (Triangle a b c m) = fmap (Triangle a b c) (wrap m)

instance Draw Triangle where
  draw Triangle {..} = for_ (polygon [triangleA, triangleB, triangleC])
    $ draw . set matrixLens triangleMatrix

triangle :: V2 Double -> V2 Double -> V2 Double -> Triangle
triangle a b c = Triangle a b c identity

bakeTriangle :: Triangle -> Triangle
bakeTriangle t@(Triangle a b c _) =
  Triangle (applyAffine t a) (applyAffine t b) (applyAffine t c) identity
