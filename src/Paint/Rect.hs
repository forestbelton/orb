module Paint.Rect (displayRect) where

import Foreign.Marshal.Alloc
import Foreign.Storable
import qualified Graphics.UI.SDL as SDL

import Paint.DisplayCommand

displayRect :: SDL.Color -> SDL.Rect -> DisplayCommand
displayRect (SDL.Color r g b a) rect ctx = do
    let re = contextRenderer ctx
    alloca $ \ptrRect -> do
        poke ptrRect rect
        SDL.setRenderDrawColor re r g b a
        SDL.renderFillRect re ptrRect
    return ()
