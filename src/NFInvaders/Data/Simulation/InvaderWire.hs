-- | Module to hold data types for the invader wire
-- invader wires represet continous time simulation of an Invader
-- they return the instance of an invader being simulated at the current frame
module NFInvaders.Data.Simulation.InvaderWire where

import Data.Set                           (Set)
import Control.Wire.Core                  (Wire)
import NFInvaders.Data.Actor.Invader      (Invader(..))
import Graphics.UI.GLFW              as G (Key(..))
import Data.Time                          (NominalDiffTime)
import Control.Wire.Session               (Timed)
import Data.Functor.Identity              (Identity)

-- | The brave defender wire uses
-- a clock frame of Timed NominalDiffTime
-- An inhibition value of ()
-- a monad transformer of Identint ( no monad transformers are used )
-- And it takes a set of keys and returns an Invader
type InvaderWire = Wire (Timed NominalDiffTime ()) () Identity (Set Key) Invader
