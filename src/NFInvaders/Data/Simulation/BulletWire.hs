-- | bullet wires represent simulation of bullets
-- they return a bullet to be rendered in a frame
module NFInvaders.Data.Simulation.BulletWire where

import Data.Set                          (Set)
import Control.Wire.Core                 (Wire)
import NFInvaders.Data.Actor.Bullet      (Bullet(..))
import Graphics.UI.GLFW             as G (Key(..))
import Data.Time                         (NominalDiffTime)
import Control.Wire.Session              (Timed)
import Data.Functor.Identity             (Identity)


-- | The brave defender wire uses
-- a clock frame of Timed NominalDiffTime
-- An inhibition value of ()
-- a monad transformer of Identint ( no monad transformers are used )
-- And it takes a set of keys and returns a Bullet
-- (doesn't need to take keys (change this))
type BulletWire = Wire (() -> Timed NominalDiffTime ()) () Identity (Set Key) Bullet
