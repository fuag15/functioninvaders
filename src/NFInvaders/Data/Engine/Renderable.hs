-- | Holds class defintion for renderable objects
module NFInvaders.Data.Engine.Renderable where

-- | simple class to represent renderable things
-- only funciton assumes the IO () is a render action
class Renderable a where
  render :: a -> IO ()
