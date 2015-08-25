module Main where

import Node
import Paint
import Layout
import Style

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as M
import System.Exit

data SDLContext = SDLContext {
    contextWindow      :: SDL.Window,
    contextRenderer    :: SDL.Renderer
 }

initScreen :: CInt -> CInt -> IO SDLContext
initScreen width height = do
    ptrWindow <- malloc
    ptrRenderer <- malloc
    SDL.createWindowAndRenderer width height 0 ptrWindow ptrRenderer
    window <- peek ptrWindow
    renderer <- peek ptrRenderer
    withCString "orb" $ \s ->
        SDL.setWindowTitle window s
    return $ SDLContext window renderer

main :: IO ()
main = do
    ctx <- initScreen 800 600
    ptrEvent <- malloc
    draw ctx
    eventLoop ptrEvent ctx

draw :: SDLContext -> IO ()
draw (SDLContext w r) = do
    SDL.setRenderDrawColor r 255 255 255 255
    SDL.renderClear r
    let dim = Dimensions (SDL.Rect 0 0 800 50) noEdges noEdges noEdges
    paint r $ buildDisplayCommands $ Node (dim, Block, Style $ M.fromAscList [(BackgroundColor, Color (SDL.Color 0 255 0 255))]) []
    SDL.renderPresent r
    return ()

eventLoop :: Ptr SDL.Event -> SDLContext -> IO ()
eventLoop pe ctx = do
    SDL.pollEvent pe
    ev <- peek pe
    case ev of
        SDL.QuitEvent _ _ -> exitSuccess
        _ -> do
            draw ctx
            eventLoop pe ctx
