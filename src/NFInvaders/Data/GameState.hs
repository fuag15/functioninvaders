{-# LANGUAGE TemplateHaskell #-}
-- | Basic game state data
module NFInvaders.Data.GameState where

import Control.Lens                                 (makeLenses)

import NFInvaders.Data.Simulation.BulletWire        (BulletWire)
import NFInvaders.Data.Simulation.InvaderWire       (InvaderWire)
import NFInvaders.Data.Simulation.BraveDefenderWire (BraveDefenderWire)

data GameState = GameState { _invaders             :: [InvaderWire]
                           , _invaderBullets       :: [BulletWire]
                           , _braveDefenderBullets :: [BulletWire]
                           , _braveDefender        :: BraveDefenderWire }

$(makeLenses ''GameState)
