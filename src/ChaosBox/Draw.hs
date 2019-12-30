module ChaosBox.Draw
  ( Draw(..)
  )
where

import           Graphics.Rendering.Cairo

-- | Class of drawable items.
--
-- 'draw' should trace the path of a shape; nothing more.
class Draw a where
  draw :: a -> Render ()
