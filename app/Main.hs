module Main where

import Paint

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign (with) 
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
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

getWindow :: SDLContext -> SDL.Window
getWindow (SDLContext window _) = window

getRenderer :: SDLContext -> SDL.Renderer
getRenderer (SDLContext _ renderer) = renderer

arial :: String
arial = "../assets/arial.ttf"

main :: IO ()
main = do
    ctx <- initScreen 800 600
    ptrEvent <- malloc
    --draw ctx
    
    TTF.withInit $ do
        font <- TTF.openFont arial 150
        textSurface <- TTF.renderUTF8Solid font "test" (SDL.Color 255 255 255 1)
        textTexture <- SDL.createTextureFromSurface (getRenderer ctx) textSurface
        loop ptrEvent ctx textTexture 

loop :: Ptr SDL.Event -> SDLContext -> SDL.Texture -> IO ()
loop event ctx textTexture = do
    let loc = SDL.Rect 320 240 150 100
    SDL.renderClear (getRenderer ctx) 
    with loc $ \loc' ->
             SDL.renderCopy (getRenderer ctx) textTexture nullPtr loc'
    SDL.renderPresent (getRenderer ctx) 
    eventLoop event ctx textTexture

draw :: SDLContext -> IO ()
draw (SDLContext w r) = do
    SDL.setRenderDrawColor r 255 255 255 255
    SDL.renderClear r
    paint r [SolidColor (SDL.Color 255 0 0 255) (SDL.Rect 0 0 800 50)]
    SDL.renderPresent r
    return ()

eventLoop :: Ptr SDL.Event -> SDLContext -> SDL.Texture -> IO ()
eventLoop pe ctx texture = do
    SDL.pollEvent pe
    ev <- peek pe
    case ev of
        SDL.QuitEvent _ _ -> exitSuccess
        _ -> do
            --draw ctx
            loop pe ctx texture 
            --eventLoop pe ctx texture
