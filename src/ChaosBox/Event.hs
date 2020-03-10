module ChaosBox.Event
  ( onClick
  )
where

import           ChaosBox.Generate
import           ChaosBox.Geometry.P2

import           Control.Monad.IO.Class
import           Control.Monad.Reader   (asks)
import qualified SDL

onClick :: MonadIO m => (P2 -> GenerateT m a) -> GenerateT m (Maybe a)
onClick act = do
  windowScale    <- asks gcScale
  SDL.P mouseLoc <- liftIO SDL.getAbsoluteMouseLocation
  checkButtons   <- liftIO SDL.getMouseButtons
  let userSpaceMouseLocation = fmap (/ windowScale) (fromIntegral <$> mouseLoc)
  if checkButtons SDL.ButtonLeft
    then Just <$> act userSpaceMouseLocation
    else pure Nothing
