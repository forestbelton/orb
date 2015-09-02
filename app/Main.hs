module Main where

import DOM
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
import System.IO
import System.Exit

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
    SDL.setRenderDrawBlendMode renderer SDL.SDL_BLENDMODE_BLEND
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
    ds <- build "./assets/test.html"
    eventLoop ptrEvent ctx ds

build :: FilePath -> IO [DisplayCommand]
build = fmap (layout . styleNode . parseDOM) . readFile

draw :: SDLContext -> [DisplayCommand] -> IO ()
draw ctx ds = do
    let r = contextRenderer ctx
    SDL.setRenderDrawColor r 255 255 255 255
    SDL.renderClear r
    paint ctx [displayRect (SDL.Color 255 255 255 255) (SDL.Rect 0 0 800 600)]
    paint ctx ds
    SDL.renderPresent r
    return ()

eventLoop :: Ptr SDL.Event -> SDLContext -> [DisplayCommand] -> IO ()
eventLoop pe ctx ds = do
    SDL.pollEvent pe
    ev <- peek pe
    case ev of
        SDL.QuitEvent _ _ -> do
            cleanUp ctx
            exitSuccess
        _ -> do
            draw ctx ds
            eventLoop pe ctx ds
