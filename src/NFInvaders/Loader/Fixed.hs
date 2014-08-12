-- | A Fixed loader for use while developing
-- bootstraps our game state
module NFInvaders.Loader.Fixed where

import NFInvaders.Data.Game                as G  ( Game(..)
                                                 , invaders
                                                 , invaderBullets
                                                 , braveDefenderBullets
                                                 , braveDefender        )

import Linear.V2                                 (V2(..))
import Control.Lens                              ((^.))
import NFInvaders.Data.Actor.Invader       as I  (Invader(..))
import NFInvaders.Data.Actor.BraveDefender as BD (BraveDefender(..))
import NFInvaders.Data.GameState           as GS (GameState(..))
import NFInvaders.Simulation.Invader             (invaderWire)
import NFInvaders.Simulation.Bullet              (bulletWire)
import NFInvaders.Simulation.BraveDefender       (braveDefenderWire)

-- | Returns a fixed initial state for the game
initialState :: GameState
initialState = makeGameState initialFrame

-- | Takes a game frame and turns it into a state simulation
makeGameState :: Game -> GameState
makeGameState frame =
  GameState { GS._invaders             = invader_wires
            , GS._invaderBullets       = invader_bullet_wires
            , GS._braveDefenderBullets = brave_defender_bullet_wires
            , GS._braveDefender        = brave_defender_wire }
  where
    invader_wires               = fmap invaderWire  $ frame ^. invaders
    invader_bullet_wires        = fmap bulletWire   $ frame ^. invaderBullets
    brave_defender_bullet_wires = fmap bulletWire   $ frame ^. braveDefenderBullets
    brave_defender_wire         = braveDefenderWire $ frame ^. braveDefender

-- | Fixed loader for development
initialFrame :: Game
initialFrame = Game { G._invaders             = [ Invader { I._position = V2 25.0 75.0
                                                          , I._health   = 1 }
                                                , Invader { I._position = V2 50.0 75.0
                                                          , I._health   = 1 }
                                                , Invader { I._position = V2 75.0 75.0
                                                          , I._health   = 1 }
                                                , Invader { I._position = V2 50.0 50.0
                                                          , I._health   = 1 } ]
                    , G._invaderBullets       = []
                    , G._braveDefenderBullets = []
                    , G._braveDefender        = BraveDefender { BD._position = V2 50.0 15.0
                                                              , BD._health   = 1 } }
