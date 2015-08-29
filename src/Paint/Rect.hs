module Paint.Rect (displayRect) where

import Foreign.Marshal.Alloc
import Foreign.Storable
import qualified Graphics.UI.SDL as SDL

import Paint.DisplayCommand

data DisplayRect = DisplayRect SDL.Color SDL.Rect

instance Display DisplayRect where
    display ctx (DisplayRect (SDL.Color r g b a) rect) = do
        let re = contextRenderer ctx
        alloca $ \ptrRect -> do
            poke ptrRect rect
            SDL.setRenderDrawColor re r g b a
            SDL.renderFillRect re ptrRect
        return ()

displayRect :: SDL.Color -> SDL.Rect -> DisplayCommand
displayRect col rect = DisplayCommand $ DisplayRect col rect
