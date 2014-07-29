-- | continous time sim of a defender
-- returns a snapshot to be rendered
module NFInvaders.Simulation.BraveDefender where

import Control.Wire.Core                            (mkSF)
import NFInvaders.Data.Actor.BraveDefender          (BraveDefender(..))
import NFInvaders.Data.Simulation.BraveDefenderWire (BraveDefenderWire)

-- | \interval keys
braveDefenderWire :: BraveDefender -> BraveDefenderWire
braveDefenderWire defender = mkSF $ \_ _ -> (defender, braveDefenderWire defender)
