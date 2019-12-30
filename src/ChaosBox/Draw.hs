module ChaosBox.Draw
  ( Draw(..)
  )
where

import           Graphics.Rendering.Cairo

-- | Class of drawable items.
--
-- 'draw' traces the path of a shape, which can then be 'fill'ed or 'stroke'd
-- using the regular cairo utilities.
class Draw a where
  draw :: a -> Render ()
