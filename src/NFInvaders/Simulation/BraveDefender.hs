{-# LANGUAGE Arrows #-}

-- | continous time sim of a defender
-- returns a snapshot to be rendered
module NFInvaders.Simulation.BraveDefender where

import FRP.Netwire                                         ( when
                                                           , pure
                                                           , (<|>)
                                                           , (.) )

import Data.Set                                            ( Set
                                                           , member )

import NFInvaders.Data.Math.Geometry                       ( Vector
                                                           , Point )

import NFInvaders.Data.Actor.BraveDefender          as BD  ( BraveDefender(..)
                                                           , position          )

import Control.Wire.Event                           as E   ( Event
                                                           , became
                                                           , now
                                                           , periodic
                                                           , once
                                                           , notYet
                                                           , (<&) )

import Control.Wire.Interval                        as I   ( after
                                                           , asSoonAs
                                                           , for
                                                           , until
                                                           , inhibit )

import NFInvaders.Data.Actor.Bullet                 as B   (Bullet(..))
import Control.Lens                                        ((^.))
import Control.Wire.Switch                                 ((-->))
import FRP.Netwire.Move                                    (integralWith)
import NFInvaders.Data.Simulation.BraveDefenderWire        (BraveDefenderWire)
import NFInvaders.Data.Simulation.GameWire                 (SimulationWire)
import NFInvaders.Util.Math                                (clampPointToBox)
import Graphics.UI.GLFW                             as G   (Key(..))
import Linear.V2                                           (V2(..))
import Prelude                                      hiding ((.))
import Control.Arrow                                       (returnA)

-- | takes an initial brave defender and returns that defender with modified position
-- Once all is well and done this should be replaced with a State and += from Control.Lens
-- for efficiency if needed
braveDefenderWire :: (Point, Point)    -- ^ box to clamp position to
                  -> BraveDefender     -- ^ initial state of the brave defender
                  -> BraveDefenderWire -- ^ wire type that generates a snapshot of a defender andany new bullets
braveDefenderWire range_box defender = proc keys_down -> do
  defender' <- move range_box defender -< keys_down
  bullets'  <- cannon                  -< (keys_down, defender' ^. position + V2 0.0 0.1)
  returnA                              -< (defender', bullets')

-- | Moves the BraveDefender and returns a new Defender
move :: (Point, Point)                           -- ^ box to clamp position to
     -> BraveDefender                            -- ^ initial state of brave defender
     -> SimulationWire (Set G.Key) BraveDefender -- ^ wire that generates a snapshot of the brave defender
move range_box defender = proc keys_down -> do
  position' <- boundedPosition range_box (defender ^. position) -< keys_down
  returnA                                                       -< BraveDefender { BD._position = position'
                                                                                 , _health      = 10       }

-- | depending on input and spawn point will either fire a bullet or not
cannon :: SimulationWire (Set G.Key, Point) [Bullet]
cannon = fireShot
      <|> pure []

rateLimitedShotFired :: SimulationWire (Set G.Key, Point) [Bullet]
rateLimitedShotFired =
  fireShot --> after 5 . rateLimitedShotFired

-- | depending on input and spawn point will either fire a bullet or not
shotFired :: SimulationWire (Set G.Key, Point) [Bullet]
shotFired =  proc (keys_down, point') -> do
  shot_fired <- fireShot -< (keys_down, point')
  I.until -< ([], shot_fired)

-- | takes the position where a bullet would be fired and keys and fires a bullet if possible
-- Brave defender is shooting takes care of cooldown
fireShot :: SimulationWire (Set G.Key) (Event (Set G.Key))
fireShot = became (member G.Key'X)
--fireShot = proc (keys_down, position') -> do
--  became (member G.Key'X) -< keys_down
--  returnA -< [Bullet { B._position  = position'
--                     , _velocity    = 25.0
--                     , _direction   = V2 0.0 1.1 }]

-- | if the player is shooting, produce a fire event
triggerPulled :: SimulationWire (Set G.Key) (Event (Set G.Key))
triggerPulled =
   became (member G.Key'X)

-- | if the player is shooting, produce a fire event
isShooting :: SimulationWire (Set G.Key) (Set G.Key)
isShooting =
  asSoonAs . became (member G.Key'X) -->
  after 0.5 . isShooting

-- | Represents a brave defenders position
boundedPosition :: (Point, Point)                   -- ^ box to clamp position to
                -> Point                            -- ^ initial position
                -> SimulationWire (Set G.Key) Point -- ^ wire that iterates position
boundedPosition range_box initial_position = proc keys_down -> do
  velocity' <- velocity                                      -< keys_down
  position' <- integralWith clampPointToBox initial_position -< (velocity', range_box)
  returnA                                                    -< position'

-- | Helper wire to determine velocity based on key presses
velocity :: SimulationWire (Set G.Key) Vector
velocity = proc keys_down -> do
  horizontal_offset <- horizontalVelocity -< keys_down
  vertical_offset   <- verticalVelocity   -< keys_down
  returnA -< V2 horizontal_offset vertical_offset

-- | Helper wire to determine horizontal velocity based on key presses
horizontalVelocity :: SimulationWire (Set G.Key) Double
horizontalVelocity =  (-20) . when (member G.Key'Left)
                  <|> 20    . when (member G.Key'Right)
                  <|> 0

-- | Helper wire to determine vertical velocity based on key presses
verticalVelocity :: SimulationWire (Set G.Key) Double
verticalVelocity =  20    . when (member G.Key'Up)
                <|> (-20) . when (member G.Key'Down)
                <|> 0
