-- | Process input from GLFW into a set of currently pressed keys
-- This should eventually be abstracted into its own class of keys
-- which will allow for different backends to send the same events to
-- the game simulations
module NFInvaders.Backend.GLFW.Input (processKeys) where

import Data.Set              ( Set
                             , insert
                             , delete )

import Graphics.UI.GLFW as G ( Key(..)
                             , KeyState(..)
                             , Window
                             , getKey )

import Control.Monad         (foldM)

-- | Get the next set of pressed keys from GLFW
processKeys :: Window       -- ^ Hanadle for window that contains the rendering window of this simulation
            -> Set Key      -- ^ Set of currently pressed keys
            -> IO (Set Key) -- ^ Returns the set of pressed keys after a sync of events with GLFW
processKeys window keys = foldM (updateKeys window) keys focusedKeys

-- | Utility function that defines keys that we care about
focusedKeys :: [Key]
focusedKeys = [ Key'Left     -- ^ Move left
              , Key'Right    -- ^ Move Right
              , Key'Up       -- ^ Move Up
              , Key'Down     -- ^ Move Down
              , Key'X        -- ^ Shoot a bullet
              , Key'Escape ] -- ^ Exit the game

-- | Takes a new state of a key we care about and updates the set of pressed keys to reflect that state
updateKeys :: Window       -- ^ Handle for window containing GLFW context we want keypresses from
           -> Set Key      -- ^ Existing key state
           -> Key          -- ^ Key to check status of
           -> IO (Set Key) -- ^ Updated key state information
updateKeys window keys key = do
  key_state <- getKey window key
  case key_state of
    KeyState'Pressed -> return $ insert key keys
    _                -> return $ delete key keys
