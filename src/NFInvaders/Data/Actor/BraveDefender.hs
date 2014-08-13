{-# LANGUAGE TemplateHaskell #-}

-- | Actor in the world representing a player
module NFInvaders.Data.Actor.BraveDefender ( BraveDefender(..)
                                           , position
                                           , health            )
where

import Control.Lens                                  ( makeLenses
                                                     , (^.)       )

import NFInvaders.Data.Engine.Bounds                 ( Bounds(..)
                                                     , Offset     )

import Graphics.Rendering.OpenGL                     ( PrimitiveMode(TriangleStrip)
                                                     , Color3(..)
                                                     , Vertex3(..)
                                                     , Vector3(..)
                                                     , GLdouble
                                                     , color
                                                     , vertex
                                                     , translate
                                                     , preservingMatrix
                                                     , renderPrimitive              )

import Linear.V2                                     (V2(..))
import NFInvaders.Data.Engine.Renderable             (Renderable(..))
import qualified NFInvaders.Data.Engine.Bounded as B (Bounded(..))
import NFInvaders.Data.Engine.Collidable             (Collidable(..))

-- | All statefull information needed to represent a player
data BraveDefender = BraveDefender { _position :: V2 Double
                                   , _health   :: Integer }

-- | Usefull lenses for modifying the defender record
$(makeLenses ''BraveDefender)

-- | Brave defender can be seen
instance Renderable BraveDefender where
  render = renderBraveDefender

-- | Use old fixed function pipeline to draw a brave defender
renderBraveDefender :: BraveDefender -> IO ()
renderBraveDefender defender = preservingMatrix $ do
  let V2 x y = defender ^. position - braveDefenderOffset
  translate (Vector3 (realToFrac x) (realToFrac y) 0 :: Vector3 GLdouble)
  renderPrimitive TriangleStrip $ do
    color  (Color3  0 1 1 :: Color3  GLdouble)
    vertex (Vertex3 0 6 0 :: Vertex3 GLdouble)
    vertex (Vertex3 0 1 0 :: Vertex3 GLdouble)
    vertex (Vertex3 1 4 0 :: Vertex3 GLdouble)
    vertex (Vertex3 4 0 0 :: Vertex3 GLdouble)
    vertex (Vertex3 4 3 0 :: Vertex3 GLdouble)
    vertex (Vertex3 8 1 0 :: Vertex3 GLdouble)
    vertex (Vertex3 7 4 0 :: Vertex3 GLdouble)
    vertex (Vertex3 8 6 0 :: Vertex3 GLdouble)

-- | Our brave defender has bounds
instance B.Bounded BraveDefender where
  bounds                  = braveDefenderBounds
  position brave_defender = brave_defender^.position

-- | Return Planar representtation of Bounds of a brave Defender
braveDefenderBounds :: BraveDefender -> (Bounds, Offset)
braveDefenderBounds _ = (Planar bound_segments, braveDefenderOffset)
  where
    bound_segments = [ (V2 0 6, V2 0 1)
                     , (V2 0 6, V2 1 4)
                     , (V2 0 1, V2 4 0)
                     , (V2 1 4, V2 4 3)
                     , (V2 4 0, V2 8 1)
                     , (V2 4 3, V2 7 5)
                     , (V2 7 5, V2 8 6)
                     , (V2 8 1, V2 8 6) ]

-- | Offset of bound definitions from center of brave defender
braveDefenderOffset :: Offset
braveDefenderOffset = V2 4 3

-- | Our defender can collide with bullets, the world, and invaders
instance Collidable BraveDefender
