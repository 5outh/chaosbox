-- | Geometry Primitives
--
-- There is, in general, one submodule per shape re-exported here.
--
-- Each shape module (@ChaosBox.Geometry.X@) defines a generalized shape
-- (@XOf@), a shape positioned by 'P2's (@X@) and a @pattern@ @X@ which allows
-- easier operation on the 'P2'-positioned shape.
--
-- Let's look at "ChaosBox.Geometry.Line" as an example (@X = Line@):
--
-- > data LineOf a = LineOf { lineStart :: a, lineEnd :: a}
--
-- 'LineOf' contains a start and end of any type @a@. This gives us 'Functor',
-- 'Foldable', and 'Traversable' instances for free.
--
-- In addition, the type synonym 'Line' is provided, since this is the most
-- common use case (A 'Line' is a line segment between two cartesian
-- coordinates):
--
-- > type Line = LineOf P2
--
-- Each module also exports a pattern for the common case. For @Line@, it is:
--
-- @
-- pattern Line :: P2 -> P2 -> Line
-- pattern Line s e = LineOf s e
-- @
--
-- This allows the user to construct 'Line's with 'Line':
--
-- @
-- myLine :: Line
-- myLine = Line (P2 0 1) (P2 10 12)
-- @
--
-- as well as pattern-match on 'Line':
--
-- @
-- midpoint :: Line -> P2
-- midPoint (Line s e) = (s + e) / 2
-- @
--
-- This is a little uncommon, but it works well here. The user can sort of
-- forget about the generalized data type when programming with the common
-- case, but the flexibility is there to generalize if and when it is needed.
--
-- All shapes instantiate the following when possible:
--
-- - 'ChaosBox.AABB.HasAABB', which provides a minimal axis-aligned bounding box for the
-- shape (see "ChaosBox.AABB")
-- - 'ChaosBox.Affine.Affine', which allows transforming the shape linearly (see
-- "ChaosBox.Affine")
-- - 'ChaosBox.Draw.Draw', which allows the shape to be drawn to the user's canvas (See
-- "ChaosBox.Draw")
--
-- "ChaosBox.Geometry.Angle" and "ChaosBox.Geometry.P2" are not shapes, but
-- useful geometric primitives nonetheless.
--
module ChaosBox.Geometry
  ( module X
  )
where

import           ChaosBox.Geometry.Angle       as X
import           ChaosBox.Geometry.Arc         as X
import           ChaosBox.Geometry.Circle      as X
import           ChaosBox.Geometry.ClosedCurve as X
import           ChaosBox.Geometry.Curve       as X
import           ChaosBox.Geometry.Ellipse     as X
import           ChaosBox.Geometry.Line        as X
import           ChaosBox.Geometry.P2          as X
import           ChaosBox.Geometry.Path        as X
import           ChaosBox.Geometry.Polygon     as X
import           ChaosBox.Geometry.Quad        as X
import           ChaosBox.Geometry.Rect        as X
import           ChaosBox.Geometry.Triangle    as X
