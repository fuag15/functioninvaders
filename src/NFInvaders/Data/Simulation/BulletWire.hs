module NFInvaders.Data.Simulation.BulletWire where

import Data.Set                          (Set)
import Control.Wire.Core                 (Wire)
import NFInvaders.Data.Actor.Bullet      (Bullet(..))
import Graphics.UI.GLFW             as G (Key(..))
import Data.Time                         (NominalDiffTime)
import Control.Wire.Session              (Timed)
import Data.Functor.Identity             (Identity)


type BulletWire = Wire (() -> Timed NominalDiffTime ()) () Identity (Set Key) Bullet
