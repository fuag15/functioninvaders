{-|
  A netwire GLFW implimentation of space invaders

  this is a work in progress

  if you want to poke around I try to keep things well documented
-}
module Main where

import NFInvaders.Backend.GLFW.Init (runGame)
import NFInvaders.Simulation.Game   (gameWire)
import NFInvaders.Loader.Fixed      (initialState)

main :: IO ()
main = runGame $ gameWire initialState
