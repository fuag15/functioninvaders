{-# LANGUAGE Arrows #-}

-- | Continous time simulation of a game returns a snapshot to be rendered each delta
module NFInvaders.Simulation.Game where

import NFInvaders.Data.GameState                                           ( GameState
                                                                           , braveDefender
                                                                           , invaderBullets
                                                                           , braveDefenderBullets
                                                                           , invaders )

import NFInvaders.Data.Simulation.GameWire                                 (GameWire)
import NFInvaders.Util.Simulation.Collection.Statefull.Generated.Filtered  (automatedWireCollection)
import NFInvaders.Simulation.Invader                                       (invaderWire)
import NFInvaders.Simulation.Bullet                                        (bulletWire)
import NFInvaders.Data.Game                                                (Game(..))
import Control.Arrow                                                       (returnA)
import Control.Lens                                                        ((^.))

-- | Gamewire that only simulates invader movement
gameWire :: GameState -> GameWire
gameWire game_state = proc keysDown -> do
  defender'             <- game_state ^. braveDefender                                              -< keysDown
  invaders'             <- automatedWireCollection invaderWire (game_state ^. invaders            ) -< []
  invaderBullets'       <- automatedWireCollection bulletWire  (game_state ^. invaderBullets      ) -< []
  braveDefenderBullets' <- automatedWireCollection bulletWire  (game_state ^. braveDefenderBullets) -< []
  returnA -< Game { _invaders             = invaders'
                  , _invaderBullets       = invaderBullets'
                  , _braveDefenderBullets = braveDefenderBullets'
                  , _braveDefender        = defender' }
