{-# LANGUAGE TemplateHaskell #-}

-- | Basic game state data
-- holds continous time simulation wires of the game
module NFInvaders.Data.GameState where

import Control.Lens                                 (makeLenses)

import NFInvaders.Data.Simulation.BulletWire        (BulletWire)
import NFInvaders.Data.Simulation.InvaderWire       (InvaderWire)
import NFInvaders.Data.Simulation.BraveDefenderWire (BraveDefenderWire)
import NFInvaders.Data.Engine.World                 (World)

-- | The current game state simulation
data GameState = GameState { _invaders             :: [InvaderWire]
                           , _invaderBullets       :: [BulletWire]
                           , _braveDefenderBullets :: [BulletWire]
                           , _braveDefender        :: BraveDefenderWire
                           , _world                :: World            }

-- | accessor functions for gamestate
$(makeLenses ''GameState)
