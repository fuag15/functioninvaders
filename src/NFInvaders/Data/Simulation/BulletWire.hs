-- | bullet wires represent simulation of bullets
-- they return a bullet to be rendered in a frame
module NFInvaders.Data.Simulation.BulletWire where

import NFInvaders.Data.Actor.Bullet        (Bullet(..))
import NFInvaders.Data.Simulation.GameWire (SimulationWire)

-- | The brave defender wire uses
-- it takes a void and returns a Bullet
type BulletWire = SimulationWire () Bullet
