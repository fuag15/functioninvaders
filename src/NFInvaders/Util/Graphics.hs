module NFInvaders.Util.Graphics where

import Graphics.Rendering.OpenGL  ( Position(..)
                                  , Size(..)
                                  , GLdouble
                                  , ortho )

setPreservingOrtho :: GLdouble
                   -> GLdouble
                   -> GLdouble
                   -> GLdouble
                   -> GLdouble
                   -> GLdouble
                   -> IO ()
setPreservingOrtho width height xmin xmax ymin ymax
  | width <= height = ortho xmin xmax (ymin * ratio) (ymax * ratio) 1 (-1)
  | otherwise       = ortho (xmin * (1 / ratio)) (xmax / ratio) ymin ymax 1 (-1)
  where ratio = height / width

viewportConfig :: Int
               -> Int
               -> (Position, Size)
viewportConfig width height
  | width <= height = (Position 0 (fromIntegral offset), Size (fromIntegral width) (fromIntegral height))
  | otherwise       = (Position (fromIntegral offset) 0, Size (fromIntegral width) (fromIntegral height))
  where offset = abs $ quot (width - height) 2
