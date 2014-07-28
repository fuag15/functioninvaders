module NFInvaders.Simulation.Invader where

import Control.Wire.Core                      (mkSF)
import NFInvaders.Data.Actor.Invader          (Invader(..))
import NFInvaders.Data.Simulation.InvaderWire (InvaderWire)

-- | \interval keys
invaderWire :: Invader -> InvaderWire
invaderWire invader = mkSF $ \_ _ -> (invader, invaderWire invader)

