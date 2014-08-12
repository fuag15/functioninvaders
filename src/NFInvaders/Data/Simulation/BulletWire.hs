-- | bullet wires represent simulation of bullets
-- they return a bullet to be rendered in a frame
module NFInvaders.Data.Simulation.BulletWire where

import Data.Set                            (Set)
import NFInvaders.Data.Actor.Bullet        (Bullet(..))
import NFInvaders.Data.Simulation.GameWire (SimulationWire)
import Graphics.UI.GLFW                    (Key(..))

-- | The brave defender wire uses
-- a clock frame of Timed NominalDiffTime
-- An inhibition value of ()
-- a monad transformer of Identint ( no monad transformers are used )
-- And it takes a set of keys and returns a Bullet
-- (doesn't need to take keys (change this))
type BulletWire = SimulationWire (Set Key) Bullet
