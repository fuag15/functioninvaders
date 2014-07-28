module NFInvaders.Backend.GLFW.Input (processKeys) where

import Data.Set              ( Set
                             , insert
                             , delete )

import Graphics.UI.GLFW as G ( Key(..)
                             , KeyState(..)
                             , Window
                             , getKey )

import Control.Monad         (foldM)

processKeys :: Window
            -> Set Key
            -> IO (Set Key)
processKeys window keys = foldM (updateKeys window) keys focusedKeys

focusedKeys :: [Key]
focusedKeys = [Key'Escape, Key'Left, Key'Right, Key'X]

updateKeys :: Window
           -> Set Key
           -> Key
           -> IO (Set Key)
updateKeys window keys key = do
  key_state <- getKey window key
  case key_state of
    KeyState'Pressed -> return $ insert key keys
    _                -> return $ delete key keys
