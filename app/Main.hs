module Main where

import Node
import Paint
import Layout
import Style

import Data.IORef
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import qualified Data.Map as M
import System.Exit

data SDLContext = SDLContext {
    contextWindow      :: SDL.Window,
    contextRenderer    :: SDL.Renderer,
    contextFontCache   :: IORef FontCache
 }

initScreen :: CInt -> CInt -> IO SDLContext
initScreen width height = do
    ptrWindow <- malloc
    ptrRenderer <- malloc
    SDL.createWindowAndRenderer width height 0 ptrWindow ptrRenderer
    window <- peek ptrWindow
    renderer <- peek ptrRenderer
    fontCache <- newIORef M.empty
    withCString "orb" $ \s ->
        SDL.setWindowTitle window s
    return $ SDLContext window renderer fontCache

cleanUp :: SDLContext -> IO ()
cleanUp ctx = do
    SDL.destroyRenderer (contextRenderer ctx)
    SDL.destroyWindow (contextWindow ctx)
    SDL.quit
    return ()

main :: IO ()
main = do
    ctx <- initScreen 800 600
    ptrEvent <- malloc
    TTF.init
    eventLoop ptrEvent ctx

arial :: String
arial = "./assets/arial.ttf"

draw :: SDLContext -> IO ()
draw ctx = do
    let r = contextRenderer ctx
    let fc = contextFontCache ctx
    SDL.setRenderDrawColor r 255 255 255 255
    SDL.renderClear r
    paint fc r $ [
        SolidColor (SDL.Color 255 0 0 255) (SDL.Rect 0 0 800 200),
        FontData (0, 0) arial 20 (SDL.Color 0 0 0 255) "test"
     ]
    --paint r $ layout $ Node (snd $ parseStyle "div { background-color: blue; }") []
    SDL.renderPresent r
    return ()

eventLoop :: Ptr SDL.Event -> SDLContext -> IO ()
eventLoop pe ctx = do
    SDL.pollEvent pe
    ev <- peek pe
    case ev of
        SDL.QuitEvent _ _ -> do
            cleanUp ctx
            exitSuccess
        _ -> do
            draw ctx
            eventLoop pe ctx
