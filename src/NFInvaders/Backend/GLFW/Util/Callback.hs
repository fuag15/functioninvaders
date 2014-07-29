-- | Holds utility functions for use with GLFW backend
module NFInvaders.Backend.GLFW.Util.Callback where

import Graphics.UI.GLFW     as G (ErrorCallback, Window)
import System.IO                 (hPutStrLn, stderr)

import NFInvaders.Util.Graphics  ( setPreservingOrtho
                                 , viewportConfig )

import Graphics.Rendering.OpenGL ( MatrixMode(Modelview, Projection)
                                 , matrixMode
                                 , loadIdentity
                                 , viewport
                                 , ($=) )

-- | Error callback that passes the error description through to stderr
error :: ErrorCallback
error _ = hPutStrLn stderr

-- | Callback to adjust our opengl framebuffer and projection whenver the window is resized
framebufferResize :: Window -- Window handle that contains framebuffer
                  -> Int    -- Width of window
                  -> Int    -- Height of window
                  -> IO ()
framebufferResize _ width height = do
  viewport   $= viewportConfig width height
  matrixMode $= Projection
  loadIdentity
  setPreservingOrtho (fromIntegral width) (fromIntegral height) 0 100 0 100
  matrixMode $= Modelview 0
