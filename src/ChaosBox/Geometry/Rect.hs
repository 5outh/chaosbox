{-# LANGUAGE ViewPatterns #-}
module ChaosBox.Geometry.Rect
  ( RectOf(..)
  , Rect
  , pattern Rect
  -- * Smart constructors
  , squareOf
  , square
  -- * Conversions
  , fromAABB
  )
where

import           ChaosBox.Prelude

import           ChaosBox.AABB
import           ChaosBox.Draw
import           ChaosBox.Geometry.Class
import           ChaosBox.Geometry.P2
import           Control.Lens            ((^.))
import           GI.Cairo.Render         hiding (Path, transform)

-- | A Rectangle
data RectOf a = RectOf
  { rectTopLeft :: a
  , rectW       :: Double
  , rectH       :: Double
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance HasP2 a => HasAABB (RectOf a) where
  aabb (RectOf tl w h) =  AABB (tl ^. _V2) w h

instance HasP2 a => Draw (RectOf a) where
  draw RectOf {..} = rectangle rectX rectY rectW rectH
    where V2 rectX rectY = rectTopLeft ^. _V2

instance HasP2 a => Boundary (RectOf a) where
  RectOf{..} `containsPoint` (V2 x y) = x >= tlx && x < brx && y >= tly && y <= bry
   where
    V2 tlx tly = rectTopLeft ^. _V2
    V2 brx bry = rectTopLeft ^. _V2 + V2 rectW rectH

squareOf :: a -> Double -> RectOf a
squareOf c w = RectOf c w w

square :: P2 -> Double -> Rect
square = squareOf

type Rect = RectOf P2

pattern Rect :: P2 -> Double -> Double -> Rect
pattern Rect c w h = RectOf c w h

fromAABB :: AABB -> Rect
fromAABB (AABB tl w h) = RectOf tl w h

--
-- There are a LOT of ways this can happen. let's enumerate them
--
-- 1. do not intersect at all: tlb and brb does not fit within x or y coordinates of tla
-- 3. top line intersects through the horizontal segments
-- 4. bottom line intersects through the horizontal segments
-- 5. left line intersects through the vertical segments
-- 6. right line intersects through the vertical segments
--
-- 2. intersect through a corner (one for each)

-- +---------+
-- |         |
-- |         |
-- |     +---*-----+
-- |     |   |     |
-- +-----*---+     |
--       |         |
--       |         |
--       |         |
--       +---------+
--

-- TODO
-- instance (HasP2 a, HasP2 b) => Intersects (RectOf a) (RectOf b) where
  -- intersectionPoints (fmap getP2 -> RectOf tla@(P2 tlax tlay) wa ha) (fmap getP2 -> RectOf tlb@(P2 tlbx tlby) wb hb) = undefined
    -- where
     -- doesNotIntersect = undefined
