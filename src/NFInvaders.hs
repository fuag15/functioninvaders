{-|
  A netwire GLFW implimentation of space invaders

  this is a work in progress

  if you want to poke around I try to keep things well documented

  Higher picture this is a prototype of a game engine in haskell using Netwire for some parts.
  The Design is meant to enable easy swapping of backends with an eye on WebGL and SDL 2 support
  Looking towards iOS and Android

  Here is a quick overview of the project layout so far

  NFInvaders.Backend.        -- ^ backen implimentations for the renderer and Input
  NFInvaders.Loader          -- ^ Things to load game state
  NFInvaders.Simulation      -- ^ Wire definitions for the game simulation
  NFInvaders.Util            -- ^ various utility functions
  NFINvaders.Data.Actor      -- ^ Data definitions of objects in the world
  NFINvaders.Data.Engine     -- ^ Class definitions for world objects
  NFINvaders.Data.Math       -- ^ Some data definitons for math related things
  NFINvaders.Data.Simulation -- ^ Definition of the simulation wires by game object type
  NFINvaders.Data.Game       -- ^ Data representing one renderable snapshot of a game simulation
  NFINvaders.Data.GameState  -- ^ Data representing a continous time simulation of the game State

  For a roadmap. check out the roadmap :)
-}
module Main where

import NFInvaders.Backend.GLFW.Init (runGame)
import NFInvaders.Simulation.Game   (gameWire)
import NFInvaders.Loader.Fixed      (initialState)

main :: IO ()
main = runGame $ gameWire initialState
