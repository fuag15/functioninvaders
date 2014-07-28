{-# LANGUAGE TemplateHaskell #-}
module NFInvaders.Data.Actor.BraveDefender ( BraveDefender(..)
                                           , position
                                           , health )
where

import Control.Lens                                  ( makeLenses
                                                     , (^.) )

import NFInvaders.Data.Engine.Bounds                 ( Bounds(..)
                                                     , Offset )

import Graphics.Rendering.OpenGL                     ( PrimitiveMode(TriangleStrip)
                                                     , Color3(..)
                                                     , Vertex3(..)
                                                     , Vector3(..)
                                                     , GLdouble
                                                     , color
                                                     , vertex
                                                     , translate
                                                     , preservingMatrix
                                                     , renderPrimitive )

import Linear.V2                                     (V2(..))
import NFInvaders.Data.Engine.Renderable             (Renderable(..))
import qualified NFInvaders.Data.Engine.Bounded as B (Bounded(..))
import NFInvaders.Data.Engine.Collidable             (Collidable(..))

data BraveDefender = BraveDefender { _position :: V2 Double
                                   , _health   :: Integer }

$(makeLenses ''BraveDefender)

instance Renderable BraveDefender where
  render = renderBraveDefender

renderBraveDefender :: BraveDefender -> IO ()
renderBraveDefender defender = preservingMatrix $ do
  let V2 x y = defender^.position - braveDefenderOffset
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

instance B.Bounded BraveDefender where
  bounds                  = braveDefenderBounds
  position brave_defender = brave_defender^.position

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

braveDefenderOffset :: Offset
braveDefenderOffset = V2 4 3

instance Collidable BraveDefender
