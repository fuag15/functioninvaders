{-# LANGUAGE Arrows #-}

-- | continous time sim of a defender
-- returns a snapshot to be rendered
module NFInvaders.Simulation.BraveDefender where

import FRP.Netwire                                       ( Wire
                                                         , when
                                                         , (<|>)
                                                         , (.)   )

import Data.Set                                          ( Set
                                                         , member )

import NFInvaders.Data.Math.Geometry                     ( Vector
                                                         , Point )

import NFInvaders.Data.Actor.BraveDefender               ( BraveDefender(..)
                                                         , position          )

import Control.Lens                                      ((^.))
import FRP.Netwire.Move                                  (integral)
import Control.Wire.Session                              (HasTime)
import NFInvaders.Data.Simulation.BraveDefenderWire      (BraveDefenderWire)
import Graphics.UI.GLFW                             as G (Key(..))
import Data.Monoid                                       (Monoid)
import Linear.V2                                         (V2(..))
import Prelude                                    hiding ((.))
import Control.Arrow                                     (returnA)

-- | takes an initial brave defender and returns that defender with modified position
-- Once all is well and done this should be replaced with a StateT and += from Control.Lens
-- for efficiency if needed
braveDefenderWire :: BraveDefender     -- ^ initial state of the brave defender
                  -> BraveDefenderWire -- ^ wire type that generates a snapshot of a defender
braveDefenderWire defender = proc keysDown -> do
  position' <- braveDefenderPosition (defender^.position) -< keysDown
  returnA                                                 -< BraveDefender { _position = position'
                                                                           , _health   = 10 }

-- | Represents a brave defenders position
braveDefenderPosition :: (HasTime t s, Monad m, Monoid e)
                      => Point                        -- ^ initial position
                      -> Wire s e m (Set G.Key) Point -- ^ wire that iterates position
braveDefenderPosition initial_position = proc keysDown -> do
  velocity  <- braveDefenderVelocity     -< keysDown
  position' <- integral initial_position -< velocity
  returnA                                -< position'


-- | Helper wire to determine velocity based on key presses
braveDefenderVelocity :: (Monad m, Monoid e)
                      => Wire s e m (Set G.Key) Vector
braveDefenderVelocity = proc keysDown -> do
  horizontal_offset <- braveDefenderHorizontalVelocity -< keysDown
  vertical_offset   <- braveDefenderVerticalVelocity   -< keysDown
  returnA                                              -< V2 horizontal_offset vertical_offset

-- | Helper wire to determine horizontal velocity based on key presses
braveDefenderHorizontalVelocity :: (Monad m, Monoid e)
                                => Wire s e m (Set G.Key) Double
braveDefenderHorizontalVelocity =  (-20) . when (member G.Key'Left)
                               <|> 20    . when (member G.Key'Right)
                               <|> 0

-- | Helper wire to determine vertical velocity based on key presses
braveDefenderVerticalVelocity :: (Monad m, Monoid e)
                              => Wire s e m (Set G.Key) Double
braveDefenderVerticalVelocity =  20    . when (member G.Key'Up)
                             <|> (-20) . when (member G.Key'Down)
                             <|> 0
