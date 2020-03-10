module ChaosBox.Geometry.Polygon
  ( PolygonOf(..)
  , Polygon
  , pattern Polygon
  , polygonOf
  , polygon
  )
where

import           ChaosBox.Prelude

import           ChaosBox.AABB
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           Control.Lens            ((^.))
import           Data.Foldable           (for_)
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NE
import           GI.Cairo.Render         hiding (Path)

-- | A closed path
newtype PolygonOf a = PolygonOf { getPolygon :: NonEmpty a }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

type Polygon = PolygonOf P2

pattern Polygon :: NonEmpty P2 -> Polygon
pattern Polygon a = PolygonOf a
{-# COMPLETE Polygon #-}

instance HasP2 a => HasAABB (PolygonOf a) where
  aabb = boundary . getPolygon

instance HasP2 a => Affine (PolygonOf a) where
  transform = defaultTransform

instance HasP2 a => Draw (PolygonOf a) where
  draw (PolygonOf (v :| rest)) = do
    let V2 startX startY = v ^. _V2
    newPath
    moveTo startX startY
    for_ (map (^. _V2) rest) (\(V2 x y) -> lineTo x y)
    closePath

polygonOf :: [a] -> Maybe (PolygonOf a)
polygonOf xs = PolygonOf <$> NE.nonEmpty xs

polygon :: [P2] -> Maybe Polygon
polygon = polygonOf
