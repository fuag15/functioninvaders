-- | Wire type for A brave defender
-- simulates the defender and returns a defneder to be rendered at the current frame
module NFInvaders.Data.Simulation.BraveDefenderWire where

import Data.Set                                 (Set)
import Control.Wire.Core                        (Wire)
import NFInvaders.Data.Actor.BraveDefender      (BraveDefender(..))
import Graphics.UI.GLFW                    as G (Key(..))
import Data.Time                                (NominalDiffTime)
import Control.Wire.Session                     (Timed)
import Data.Functor.Identity                    (Identity)

-- | The brave defender wire uses
-- a clock frame of Timed NominalDiffTime
-- An inhibition value of ()
-- a monad transformer of Identint ( no monad transformers are used )
-- And it takes a set of keys and returns a BraveDefender
type BraveDefenderWire = Wire (() -> Timed NominalDiffTime ()) () Identity (Set Key) BraveDefender
