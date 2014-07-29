-- | Initialization boilerpplate for GLFW
module NFInvaders.Backend.GLFW.Init where

import NFInvaders.Backend.GLFW.Util.Callback as CB (error, framebufferResize)
import NFInvaders.Backend.GLFW.Loop                (mainLoop)
import Control.Monad                         as M  (unless)
import System.Exit                                 (exitSuccess, exitFailure)
import Data.Set                                    (empty)

import Graphics.UI.GLFW                      as G  ( init
                                                   , setFramebufferSizeCallback
                                                   , setErrorCallback
                                                   , swapInterval
                                                   , createWindow
                                                   , makeContextCurrent
                                                   , destroyWindow
                                                   , terminate )

import NFInvaders.Data.Simulation.GameWire         (GameWire)
import Control.Wire.Session                        (clockSession)

-- | Takes a GameWire (Game Simulation) and initializes GLFW context to run it in
runGame :: GameWire -- ^ Simulation of game in wire form, returns a frame each simstep
        -> IO ()
runGame game_wire = do
  setErrorCallback $ Just CB.error
  successful_init <- G.init
  M.unless successful_init exitFailure
  window_handle <- createWindow 640 480 "Function Invaders" Nothing Nothing
  case window_handle of
    Nothing     -> terminate >> exitFailure
    Just window -> do
      makeContextCurrent window_handle
      swapInterval 1
      setFramebufferSizeCallback window $ Just CB.framebufferResize
      mainLoop window empty game_wire clockSession
      destroyWindow window
      terminate
      exitSuccess
