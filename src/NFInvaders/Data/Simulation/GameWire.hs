module NFInvaders.Data.Simulation.GameWire where

import Data.Set                                  (Set)
import Control.Wire.Core                         (Wire)
import Graphics.UI.GLFW                     as G (Key(..))
import Data.Time                                 (NominalDiffTime)
import Control.Wire.Session                      (Session, Timed)
import Data.Functor.Identity                     (Identity)
import NFInvaders.Data.Game                      (Game)

type GameSession = Session IO (() -> Timed NominalDiffTime ())
type GameWire = Wire (() -> Timed NominalDiffTime ()) () Identity (Set Key) Game
