-- | module holds the game wire
-- the game wire is a continous time simulation of the game
-- it returns a frame to render on each step
module NFInvaders.Data.Simulation.GameWire where

import Data.Set                                  (Set)
import Control.Wire.Core                         (Wire)
import Graphics.UI.GLFW                     as G (Key(..))
import Data.Time                                 (NominalDiffTime)
import Control.Wire.Session                      (Session, Timed)
import Data.Functor.Identity                     (Identity)
import NFInvaders.Data.Game                      (Game)

-- | The game runs on seesion that have IO (they talk to the clock)
-- these seesions take anything nad return a Timed NominalDiffTime
type GameSession = Session IO (Timed NominalDiffTime ())

-- | Base definition of the wire type chosen for haskell invaders
-- a clock frame of Timed NominalDiffTime
-- An inhibition value of ()
-- a monad transformer of Identint ( no monad transformers are used )
type SimulationWire = Wire (Timed NominalDiffTime ()) () Identity

-- | The brave defender wire uses
-- takes a set of keys and returns a Game
type GameWire = SimulationWire (Set G.Key) Game
