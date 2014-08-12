-- | Module to hold data types for the invader wire
-- invader wires represet continous time simulation of an Invader
-- they return the instance of an invader being simulated at the current frame
module NFInvaders.Data.Simulation.InvaderWire where

import NFInvaders.Data.Actor.Invader       (Invader(..))
import NFInvaders.Data.Simulation.GameWire (SimulationWire)

-- | The brave defender wire uses
-- And it takes nothing meaningfull returns an Invader
type InvaderWire = SimulationWire () Invader
