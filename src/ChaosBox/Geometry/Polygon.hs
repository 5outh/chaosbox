module ChaosBox.Geometry.Polygon
  ( PolygonOf(..)
  , Polygon
  , polygon
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Geometry.P2
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.AABB
import           ChaosBox.Geometry.Class
import           Control.Lens                   ( (^.) )
import           Data.Foldable                  ( for_ )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Graphics.Rendering.Cairo
                                         hiding ( Path )

-- | A closed path
newtype PolygonOf a = PolygonOf { getPolygon :: NonEmpty a }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

type Polygon = PolygonOf P2

instance HasV2 a => HasAABB (PolygonOf a) where
  aabb = boundary . getPolygon

instance HasV2 a => Affine (PolygonOf a) where
  transform = defaultTransform

instance HasV2 a => Draw (PolygonOf a) where
  draw (PolygonOf (v :| rest)) = do
    let V2 startX startY = v ^. _V2
    newPath
    moveTo startX startY
    for_ (map (^. _V2) rest) (\(V2 x y) -> lineTo x y)
    closePath

polygon :: [a] -> Maybe (PolygonOf a)
polygon xs = PolygonOf <$> NE.nonEmpty xs
