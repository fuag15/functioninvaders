module NFInvaders.Data.Simulation.InvaderWire where

import Data.Set                           (Set)
import Control.Wire.Core                  (Wire)
import NFInvaders.Data.Actor.Invader      (Invader(..))
import Graphics.UI.GLFW              as G (Key(..))
import Data.Time                          (NominalDiffTime)
import Control.Wire.Session               (Timed)
import Data.Functor.Identity              (Identity)


type InvaderWire = Wire (() -> Timed NominalDiffTime ()) () Identity (Set Key) Invader
