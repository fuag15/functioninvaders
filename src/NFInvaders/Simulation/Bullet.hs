-- | Continous time simulation of a bullet
-- reterns a snapshot to be rendered
module NFInvaders.Simulation.Bullet where

import Control.Wire.Core                          (mkSF)
import NFInvaders.Data.Actor.Bullet               (Bullet(..))
import NFInvaders.Data.Simulation.BulletWire      (BulletWire)

-- | \interval keys
bulletWire :: Bullet -> BulletWire
bulletWire bullet = mkSF $ \_ _ -> (bullet, bulletWire bullet)
