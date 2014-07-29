-- | Main render loop for the GLFW backend
module NFInvaders.Backend.GLFW.Loop where

import Graphics.Rendering.OpenGL                ( ClearBuffer(ColorBuffer)
                                                , loadIdentity
                                                , clear )

import Graphics.UI.GLFW                    as G ( Key(Key'Escape)
                                                , Window
                                                , swapBuffers
                                                , pollEvents )

import NFInvaders.Data.Engine.Renderable   as R (Renderable(render))
import NFInvaders.Backend.GLFW.Input            (processKeys)
import NFInvaders.Data.Simulation.GameWire      (GameSession, GameWire)
import Control.Monad                       as M (unless)
import Data.Set                                 (Set, member)
import Control.Wire.Core                        (stepWire)
import Control.Wire.Session                     (stepSession)
import Data.Functor.Identity                    (runIdentity)

mainLoop :: G.Window     -- ^ Handle to window where our framebuffer lives
         -> Set Key      -- ^ Set of currently pressed keys
         -> GameWire     -- ^ Game Simulation Wire, takes time deltas, outputs frames
         -> GameSession  -- ^ Type of clock that generates timedeltas for the GameWire
         -> IO ()
mainLoop window keys game_wire session = do
  keys' <- processKeys window keys
  M.unless (member G.Key'Escape keys') $ do
    (interval, session') <- stepSession session
    let (Right renderables, game_wire') = runIdentity $ stepWire game_wire interval $ Right keys'
    clear [ColorBuffer]
    loadIdentity
    render renderables
    swapBuffers window
    pollEvents
    mainLoop window keys' game_wire' session'
