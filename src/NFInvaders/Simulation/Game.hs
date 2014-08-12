{-# LANGUAGE Arrows #-}

-- | Continous time simulation of a game returns a snapshot to be rendered each delta
module NFInvaders.Simulation.Game where

import NFInvaders.Data.GameState                                          as GS ( GameState(..)
                                                                                , braveDefender
                                                                                , invaders )

import NFInvaders.Data.Simulation.GameWire                                      (GameWire)
import NFInvaders.Util.Simulation.Collection.Statefull.Generated.Filtered       (automatedWireCollection)
import NFInvaders.Simulation.Invader                                            (invaderWire)

-- Temporary, remove this
import NFInvaders.Data.Game                                                as G ( Game(..)
                                                                                , invaderBullets
                                                                                , braveDefenderBullets )

import NFInvaders.Loader.Fixed                                                  (initialFrame)
import Control.Arrow                                                            (returnA)
import Control.Lens                                                             ((^.))

-- | Gamewire that only simulates invader movement
gameWire :: GameState -> GameWire
gameWire game_state = proc keysDown -> do
  defender' <- game_state^.braveDefender                                  -< keysDown
  invaders' <- automatedWireCollection invaderWire (game_state^.invaders) -< []
  returnA                                                                 -< Game { G._invaders             = invaders'
                                                                                  , G._invaderBullets       = initialFrame^.invaderBullets
                                                                                  , G._braveDefenderBullets = initialFrame^.braveDefenderBullets
                                                                                  , G._braveDefender        = defender' }
