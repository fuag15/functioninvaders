-- | simple input function utilities
module NFInvaders.Util.Input where

import Data.Set         as S ( Set
                             , member )

import Graphics.UI.GLFW as G (Key(..))

-- | Helper to filter set of keys and check for presence
keyDown :: G.Key     -- ^ Key to check for
        -> Set G.Key -- ^ Set to check presence in
        -> Bool
keyDown = S.member
