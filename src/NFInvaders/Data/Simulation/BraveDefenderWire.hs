-- | Wire type for A brave defender
-- simulates the defender and returns a defneder to be rendered at the current frame
module NFInvaders.Data.Simulation.BraveDefenderWire where

import Data.Set                            (Set)
import NFInvaders.Data.Actor.BraveDefender (BraveDefender)
import NFInvaders.Data.Actor.Bullet        (Bullet)
import NFInvaders.Data.Simulation.GameWire (SimulationWire)
import Graphics.UI.GLFW                    (Key(..))

-- | The brave defender wire uses
-- a clock frame of Timed NominalDiffTime
-- An inhibition value of ()
-- a monad transformer of Identint ( no monad transformers are used )
-- And it takes a set of keys and returns a BraveDefender and any bullets the defender fired
type BraveDefenderWire = SimulationWire (Set Key) (BraveDefender, [Bullet])
