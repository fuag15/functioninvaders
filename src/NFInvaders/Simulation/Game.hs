{-# LANGUAGE Arrows #-}

-- | Continous time simulation of a game returns a snapshot to be rendered each delta
module NFInvaders.Simulation.Game where

import NFInvaders.Data.GameState           as GS ( GameState(..)
                                                 , braveDefender )

import NFInvaders.Data.Simulation.GameWire       (GameWire)

-- Temporary, remove this
import NFInvaders.Loader.Fixed                   (initialFrame)
import Control.Arrow                             (returnA)
import NFInvaders.Data.Game                 as G ( Game(..)
                                                 , invaders
                                                 , invaderBullets
                                                 , braveDefenderBullets )
import Control.Lens                              ((^.))

-- | Gamewire that only simulates invader movement
gameWire :: GameState -> GameWire
gameWire game_state = proc keysDown -> do
  defender' <- game_state^.braveDefender -< keysDown
  returnA                                -< Game { G._invaders             = initialFrame^.invaders
                                                 , G._invaderBullets       = initialFrame^.invaderBullets
                                                 , G._braveDefenderBullets = initialFrame^.braveDefenderBullets
                                                 , G._braveDefender        = defender' }
