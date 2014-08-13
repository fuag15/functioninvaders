{-# LANGUAGE TemplateHaskell #-}

-- | Enemy Invaders of our galaxy!
module NFInvaders.Data.Actor.Invader ( Invader(..)
                                     , position
                                     , health     )
where

import Control.Lens                                  ( makeLenses
                                                     , (^.)       )

import NFInvaders.Data.Engine.Bounds                 ( Bounds(..)
                                                     , Offset     )

import Graphics.Rendering.OpenGL                     ( PrimitiveMode(TriangleFan)
                                                     , Color3(..)
                                                     , Vertex3(..)
                                                     , Vector3(..)
                                                     , GLdouble
                                                     , color
                                                     , vertex
                                                     , translate
                                                     , preservingMatrix
                                                     , renderPrimitive            )

import Linear.V2                                     (V2(..))
import NFInvaders.Data.Engine.Renderable             (Renderable(..))
import qualified NFInvaders.Data.Engine.Bounded as B (Bounded(..))
import NFInvaders.Data.Engine.Collidable             (Collidable(..))

-- | Statefull information about an Invader
data Invader = Invader { _position :: V2 Double
                       , _health   :: Integer }

-- | Usefull accessors / modifiers
$(makeLenses ''Invader)

-- | Invaders can be seen
instance Renderable Invader where
  render = renderInvader

-- | Olde fixed function pipelin rendering function for Invaders
renderInvader :: Invader -> IO ()
renderInvader invader = preservingMatrix $ do
  let V2 x y = invader ^. position - invaderOffset
  translate (Vector3 (realToFrac x) (realToFrac y) 0 :: Vector3 GLdouble)
  renderPrimitive TriangleFan $ do
    color  (Color3  1 0 1 :: Color3  GLdouble)
    vertex (Vertex3 2 0 0 :: Vertex3 GLdouble)
    vertex (Vertex3 0 6 0 :: Vertex3 GLdouble)
    vertex (Vertex3 1 5 0 :: Vertex3 GLdouble)
    vertex (Vertex3 2 6 0 :: Vertex3 GLdouble)
    vertex (Vertex3 3 5 0 :: Vertex3 GLdouble)
    vertex (Vertex3 4 6 0 :: Vertex3 GLdouble)

-- | Invaders have bounds
instance B.Bounded Invader where
  bounds           = invaderBounds
  position invader = invader^.position

-- | Planar bounds for invaders (line segments that match the drawable)
invaderBounds :: Invader -> (Bounds, Offset)
invaderBounds _ = (Planar bound_segments, invaderOffset)
  where
    bound_segments = [ (V2 2 0, V2 0 6)
                     , (V2 0 6, V2 1 5)
                     , (V2 1 5, V2 2 6)
                     , (V2 2 6, V2 3 5)
                     , (V2 3 5, V2 4 6)
                     , (V2 4 6, V2 2 0) ]

-- | Offset of invader bounds from the center of the invader
invaderOffset :: Offset
invaderOffset = V2 2 3

-- | Invaders can collide
instance Collidable Invader
