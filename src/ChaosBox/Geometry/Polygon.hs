module ChaosBox.Geometry.Polygon
  ( PolygonOf(..)
  , Polygon
  , polygon
  , polygonOf
  )
where

import           ChaosBox.Prelude

import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.HasV2
import           Control.Lens             ((^.))
import           Data.Foldable            (for_)
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Graphics.Rendering.Cairo hiding (Path)

-- | A closed path
newtype PolygonOf a = PolygonOf { getPolygonOf :: NonEmpty a }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

type Polygon = PolygonOf (V2 Double)

instance HasV2 a => Affine (PolygonOf a) where
  transform = defaultTransform

instance HasV2 a => Draw (PolygonOf a) where
  draw (PolygonOf (v :| rest)) = do
    let V2 startX startY = v ^. _V2
    newPath
    moveTo startX startY
    for_ (map (^. _V2) rest) (\(V2 x y) -> lineTo x y)
    closePath

polygon :: [V2 Double] -> Maybe Polygon
polygon = polygonOf

polygonOf :: [a] -> Maybe (PolygonOf a)
polygonOf xs = PolygonOf <$> NE.nonEmpty xs
