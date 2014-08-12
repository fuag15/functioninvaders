{-# LANGUAGE Arrows #-}

-- | Invader simulation returns a sanpshot of the invader each delta
module NFInvaders.Simulation.Invader where


import FRP.Netwire                            ( pure
                                              , (.) )

import NFInvaders.Data.Actor.Invader          ( Invader(..)
                                              , position )

import NFInvaders.Data.Math.Geometry          ( Vector
                                              , Point )

import Control.Wire.Interval                  (for)
import FRP.Netwire.Move                       (integral)
import Control.Wire.Switch                    ((-->))
import Control.Lens                           ((^.))
import Control.Arrow                          (returnA)
import NFInvaders.Data.Simulation.InvaderWire (InvaderWire)
import NFInvaders.Data.Simulation.GameWire    (SimulationWire)
import Linear.Vector                          ((^*))
import Linear.V2                              (V2(..))
import Data.Time                              (NominalDiffTime)
import Prelude                         hiding ((.))

-- | Takes an initial invader and returns that invader with modified position each sim step
-- Once all is well and done this should be replaced with a State and += from Control.Lens
-- for efficiency if needed
invaderWire :: Invader     -- ^ initial stateo of invader
            -> InvaderWire -- ^ wire type that generates a snapshot of an invader each simulation step
invaderWire invader = proc _ -> do
  position' <- invaderPosition (invader^.position) -< ()
  returnA                                          -< Invader { _position = position'
                                                              , _health   = 10        }

-- | Represents an invaders position
invaderPosition :: Point                   -- ^ initial position
                -> SimulationWire () Point -- ^ wire that iterates position
invaderPosition initial_position =
  integral initial_position . invaderVelocityCycle 1.0

-- | One cycle worth of an invaders velocity
invaderVelocityCycle :: NominalDiffTime          -- ^ Total time for one cycle
                     -> SimulationWire () Vector -- ^ Wire that gives a velocity that loops through all directions in the time give
invaderVelocityCycle duration =
  step (V2 (-1) 0   ) -->       -- step Left
  step (V2 0    (-1)) -->       -- step Down
  step (V2 1    0   ) -->       -- step Up
  step (V2 0    1   ) -->       -- step Right
  invaderVelocityCycle duration -- repeat
  where
    step = invaderTimedVelocityVector (duration / 4) 20

-- | Wire representing a timed velocity along a set vector
invaderTimedVelocityVector :: NominalDiffTime          -- ^ The amount of time to maintain this velocity
                           -> Double                   -- ^ the amount velocity to apply in the direction
                           -> Vector                   -- ^ The vector representing the directon of velocity
                           -> SimulationWire () Vector -- ^ This wire takes anything and returns a Vector
invaderTimedVelocityVector duration magnitude direction =
  for duration . pure (direction ^* magnitude)
