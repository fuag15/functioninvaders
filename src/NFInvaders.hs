-- | Simple space invaders using netwire and sdl
module Main where

import NFInvaders.Backend.GLFW.Init (runGame)
import NFInvaders.Simulation.Game   (gameWire)
import NFInvaders.Loader.Fixed      (initialState)

main :: IO ()
main = runGame $ gameWire initialState
