-- | Utility functions for dealing with the viewport
module NFInvaders.Util.Graphics where

import Graphics.Rendering.OpenGL  ( Position(..)
                                  , Size(..)
                                  , GLdouble
                                  , ortho       )

-- | set an maximal orthographic projection while preserving aspect ratio
setPreservingOrtho :: GLdouble -- ^ width of the window
                   -> GLdouble -- ^ height of the window
                   -> GLdouble -- ^ minimum x dimensions of the scene
                   -> GLdouble -- ^ maximum x dimensions of the scene
                   -> GLdouble -- ^ minimum y dimensions of the scene
                   -> GLdouble -- ^ maximum y dimensions of the scene
                   -> IO ()    -- ^ this function alters the projection matrix
setPreservingOrtho width height xmin xmax ymin ymax
  | width <= height = ortho xmin xmax (ymin * ratio) (ymax * ratio) 1 (-1)
  | otherwise       = ortho (xmin * (1 / ratio)) (xmax / ratio) ymin ymax 1 (-1)
  where ratio = height / width

-- | Given a width and height of the viewport determine the maximum sized centerd square position in which to render the game
viewportConfig :: Int              -- ^ Width of the viewport
               -> Int              -- ^ height of the viewport
               -> (Position, Size) -- ^ resulting positional offset and size of the maximum square
viewportConfig width height
  | width <= height = (Position 0 (fromIntegral offset), Size (fromIntegral width) (fromIntegral height))
  | otherwise       = (Position (fromIntegral offset) 0, Size (fromIntegral width) (fromIntegral height))
  where offset = abs $ quot (width - height) 2
