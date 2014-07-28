module NFInvaders.Data.Simulation.BraveDefenderWire where

import Data.Set                                 (Set)
import Control.Wire.Core                        (Wire)
import NFInvaders.Data.Actor.BraveDefender      (BraveDefender(..))
import Graphics.UI.GLFW                    as G (Key(..))
import Data.Time                                (NominalDiffTime)
import Control.Wire.Session                     (Timed)
import Data.Functor.Identity                    (Identity)


type BraveDefenderWire = Wire (() -> Timed NominalDiffTime ()) () Identity (Set Key) BraveDefender
