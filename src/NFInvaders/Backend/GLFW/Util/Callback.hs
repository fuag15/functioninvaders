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

error :: ErrorCallback
error _ = hPutStrLn stderr

framebufferResize :: Window
                  -> Int
                  -> Int
                  -> IO ()
framebufferResize _ width height = do
  viewport   $= viewportConfig width height
  matrixMode $= Projection
  loadIdentity
  setPreservingOrtho (fromIntegral width) (fromIntegral height) 0 100 0 100
  matrixMode $= Modelview 0
