{-# LANGUAGE Arrows #-}

-- | Continous time simulation of a game returns a snapshot to be rendered each delta
module NFInvaders.Simulation.Game where

import NFInvaders.Data.GameState                                           ( GameState
                                                                           , braveDefender
                                                                           , invaderBullets
                                                                           , braveDefenderBullets
                                                                           , invaders
                                                                           , world )

import NFInvaders.Data.Simulation.GameWire                                 (GameWire)
import NFInvaders.Util.Simulation.Collection.Statefull.Generated.Filtered  (automatedWireCollection)
import NFInvaders.Simulation.Invader                                       (invaderWire)
import NFInvaders.Simulation.Bullet                                        (bulletWire)
import NFInvaders.Data.Game                                                (Game(..))
import Control.Arrow                                                       (returnA)
import Control.Lens                                                        ((^.))

-- | Gamewire that only simulates invader movement
gameWire :: GameState -> GameWire
gameWire game_state = proc keys_down -> do
  (defender', bullets') <- game_state ^. braveDefender                                              -< keys_down
  invaders'             <- automatedWireCollection invaderWire (game_state ^. invaders            ) -< []
  invaderBullets'       <- automatedWireCollection bulletWire  (game_state ^. invaderBullets      ) -< []
  braveDefenderBullets' <- automatedWireCollection bulletWire  (game_state ^. braveDefenderBullets) -< bullets'
  returnA -< Game { _invaders             = invaders'
                  , _invaderBullets       = invaderBullets'
                  , _braveDefenderBullets = braveDefenderBullets'
                  , _braveDefender        = defender'
                  , _world                = game_state ^. world }
