module NFInvaders.Simulation.Game where

import Control.Wire.Core                         (mkSF)
import NFInvaders.Data.GameState                 (GameState(..))
import NFInvaders.Data.Simulation.GameWire       (GameWire)

-- Temporary, remove this
import NFInvaders.Loader.Fixed                   (initialFrame)

-- | \interval keys
gameWire :: GameState -> GameWire
gameWire game_state = mkSF $ \_ _ -> (game_frame, gameWire game_state)
  where
    game_frame = initialFrame
