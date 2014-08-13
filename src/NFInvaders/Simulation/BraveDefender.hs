{-# LANGUAGE Arrows #-}

-- | continous time sim of a defender
-- returns a snapshot to be rendered
module NFInvaders.Simulation.BraveDefender where

import FRP.Netwire                                       ( when
                                                         , (<|>)
                                                         , (.)   )

import Data.Set                                          ( Set
                                                         , member )

import NFInvaders.Data.Math.Geometry                     ( Vector
                                                         , Point )

import NFInvaders.Data.Actor.BraveDefender               ( BraveDefender(..)
                                                         , position          )

import Control.Lens                                      ((^.))
import FRP.Netwire.Move                                  (integralWith)
import NFInvaders.Data.Simulation.BraveDefenderWire      (BraveDefenderWire)
import NFInvaders.Data.Simulation.GameWire               (SimulationWire)
import NFInvaders.Util.Math                              (clampPointToBox)
import Graphics.UI.GLFW                             as G (Key(..))
import Linear.V2                                         (V2(..))
import Prelude                                    hiding ((.))
import Control.Arrow                                     (returnA)

-- | takes an initial brave defender and returns that defender with modified position
-- Once all is well and done this should be replaced with a State and += from Control.Lens
-- for efficiency if needed
braveDefenderWire :: (Point, Point)    -- ^ box to clamp position to
                  -> BraveDefender     -- ^ initial state of the brave defender
                  -> BraveDefenderWire -- ^ wire type that generates a snapshot of a defender
braveDefenderWire range_box defender = proc keys_down -> do
  position' <- braveDefenderPosition range_box (defender ^. position) -< keys_down
  returnA                                                             -< BraveDefender { _position = position'
                                                                                       , _health   = 10       }

-- | Represents a brave defenders position
braveDefenderPosition :: (Point, Point)                   -- ^ box to clamp position to
                      -> Point                            -- ^ initial position
                      -> SimulationWire (Set G.Key) Point -- ^ wire that iterates position
braveDefenderPosition range_box initial_position = proc keys_down -> do
  velocity  <- braveDefenderVelocity                         -< keys_down
  position' <- integralWith clampPointToBox initial_position -< (velocity, range_box)
  returnA                                                    -< position'

-- | Helper wire to determine velocity based on key presses
braveDefenderVelocity :: SimulationWire (Set G.Key) Vector
braveDefenderVelocity = proc keys_down -> do
  horizontal_offset <- braveDefenderHorizontalVelocity -< keys_down
  vertical_offset   <- braveDefenderVerticalVelocity   -< keys_down
  returnA -< V2 horizontal_offset vertical_offset

-- | Helper wire to determine horizontal velocity based on key presses
braveDefenderHorizontalVelocity :: SimulationWire (Set G.Key) Double
braveDefenderHorizontalVelocity =  (-20) . when (member G.Key'Left)
                               <|> 20    . when (member G.Key'Right)
                               <|> 0

-- | Helper wire to determine vertical velocity based on key presses
braveDefenderVerticalVelocity :: SimulationWire (Set G.Key) Double
braveDefenderVerticalVelocity =  20    . when (member G.Key'Up)
                             <|> (-20) . when (member G.Key'Down)
                             <|> 0
