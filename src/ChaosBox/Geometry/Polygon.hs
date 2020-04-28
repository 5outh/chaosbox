-- | Arbitrary polygons
module ChaosBox.Geometry.Polygon
  ( PolygonOf(..)
  , Polygon
  , pattern Polygon
  , getPolygon
  , polygonOf
  , polygon
  , polygonCenter
  , translatePolygon
  , scalePolygon
  , scalePolygonAround
  , rotatePolygon
  , rotatePolygonAround
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Math (average)
import           ChaosBox.AABB
import           ChaosBox.Draw
import           ChaosBox.Geometry.Angle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Transform
import           Control.Lens                ((^.))
import           Data.Foldable               (for_)
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NE
import           GI.Cairo.Render             hiding (Path)

-- | A closed path
newtype PolygonOf a = PolygonOf { getPolygonOf :: NonEmpty a }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

type Polygon = PolygonOf P2

pattern Polygon :: NonEmpty P2 -> Polygon
pattern Polygon {getPolygon} = PolygonOf getPolygon
{-# COMPLETE Polygon #-}

instance HasP2 a => HasAABB (PolygonOf a) where
  aabb = boundary . getPolygonOf

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

translatePolygon :: HasP2 a => P2 -> PolygonOf a -> PolygonOf a
translatePolygon = translatePoints

scalePolygon :: HasP2 a => P2 -> PolygonOf a -> PolygonOf a
scalePolygon = scalePoints

scalePolygonAround :: HasP2 a => P2 -> P2 -> PolygonOf a -> PolygonOf a
scalePolygonAround = scaleAroundPoints

rotatePolygon :: HasP2 a => Angle -> PolygonOf a -> PolygonOf a
rotatePolygon = rotatePoints

rotatePolygonAround :: HasP2 a => P2 -> Angle -> PolygonOf a -> PolygonOf a
rotatePolygonAround = rotateAroundPoints

-- | The center of mass of a 'Polygon'
polygonCenter :: HasP2 a => PolygonOf a -> P2
polygonCenter = average . fmap (^._V2)
