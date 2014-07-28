module NFInvaders.Data.Engine.Renderable where

class Renderable a where
  render :: a -> IO ()
