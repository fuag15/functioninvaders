{-# LANGUAGE TemplateHaskell #-}

-- | Definition of a bullet actor in space invadors
module NFInvaders.Data.Actor.Bullet where

import Control.Lens                                  ( makeLenses
                                                     , (^.)       )

import Graphics.Rendering.OpenGL                     ( PrimitiveMode(Points)
                                                     , Color3(..)
                                                     , Vertex3(..)
                                                     , Vector3(..)
                                                     , GLdouble
                                                     , color
                                                     , vertex
                                                     , translate
                                                     , preservingMatrix
                                                     , renderPrimitive       )

import NFInvaders.Data.Engine.Renderable             (Renderable(..))
import qualified NFInvaders.Data.Engine.Bounded as B (Bounded(..))
import NFInvaders.Data.Engine.Bounds                 (Bounds(..))
import NFInvaders.Data.Engine.Collidable             (Collidable(..))
import Linear.V2                                     (V2(..))

-- | Bullets have a position, velocity and direction
data Bullet = Bullet { _position  :: V2 Double
                     , _velocity  :: V2 Double
                     , _direction :: V2 Double }

-- | Useful accessors / modifiers for bullets
$(makeLenses ''Bullet)

-- | Bullets can be rendered
instance Renderable Bullet where
  render = renderBullet

-- | Uses old fixed function pipe to render a bullet
renderBullet :: Bullet -> IO ()
renderBullet bullet = preservingMatrix $ do
  let V2 x y = bullet^.position
  translate (Vector3 (realToFrac x) (realToFrac y) 0 :: Vector3 GLdouble)
  renderPrimitive Points $ do
    color  (Color3  1 1 0 :: Color3 GLdouble)
    vertex (Vertex3 0 0 0 :: Vertex3 GLdouble)

-- | bullets have bounds (just a point)
instance B.Bounded Bullet where
  bounds   _      = (Only (V2 0 0), V2 0 0)
  position bullet = bullet^.position

-- | Bullets can collide with other entities
instance Collidable Bullet
