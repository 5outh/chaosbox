-- | Arcs (partial circles)
module ChaosBox.Geometry.Arc
  ( ArcOf(..)
  , Arc
  , pattern Arc
  , arc
  , arcOf
  , arcPoints
  , arcCenter
  ,arcRadius
  ,arcStart
  ,arcEnd
  ,arcDetail
  )
where

import           ChaosBox.Prelude        hiding (unit)

import           ChaosBox.AABB
import           ChaosBox.Affine
import           ChaosBox.Draw
import           ChaosBox.Geometry.Angle
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           ChaosBox.Geometry.Path
import           ChaosBox.Math           (lerpMany)
import           Control.Lens            ((&), (.~), (^.))
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified GI.Cairo.Render         as Cairo

-- | Arc (partial Circle)
data ArcOf a = ArcOf
  { arcOfCenter :: a
  -- ^ Center of the arc's circle
  , arcOfRadius :: Double
  -- ^ Radius of the arc's circle
  , arcOfStart  :: Angle
  -- ^ Start 'Angle'
  , arcOfEnd    :: Angle
  -- ^ End 'Angle'
  , arcOfDetail :: Int
  -- ^ Detail in number of points
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Arc = ArcOf P2

pattern Arc :: P2 -> Double -> Angle -> Angle -> Int -> Arc
pattern Arc {arcCenter,arcRadius,arcStart,arcEnd,arcDetail} = ArcOf arcCenter arcRadius arcStart arcEnd arcDetail
{-# COMPLETE Arc #-}

instance HasP2 a => Draw (ArcOf a) where
  draw ArcOf {..} = Cairo.arc x
                              y
                              arcOfRadius
                              (getAngle arcOfStart)
                              (getAngle arcOfEnd)
    where V2 x y = arcOfCenter ^. _V2

instance HasP2 a => Affine (ArcOf a) where
  type Transformed (ArcOf a) = Maybe (PathOf a)
  transform m = fmap (transform m) . pathOf . arcPoints

instance HasP2 a => HasAABB (ArcOf a) where
  aabb ArcOf {..} = boundary $ p1 :| [p2]
   where
    c  = arcOfCenter ^. _V2
    p1 = c + unit arcOfStart ^* arcOfRadius
    p2 = c + unit arcOfEnd ^* arcOfRadius

-- | An 'Arc' with default detail (200)
arcOf :: a -> Double -> Angle -> Angle -> ArcOf a
arcOf c r s e = ArcOf c r s e 200

-- | An 'Arc' with default detail (200)
arc :: P2 -> Double -> Angle -> Angle -> Arc
arc = arcOf @P2

arcPoints :: HasP2 a => ArcOf a -> [a]
arcPoints ArcOf {..} = points
 where
  angles = lerpMany arcOfDetail arcOfStart arcOfEnd
  points = flip map angles $ \theta ->
    arcOfCenter & _V2 .~ (arcOfCenter ^. _V2 + (unit theta ^* arcOfRadius))
