module Paint (DisplayCommand(..), paint) where

import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

import System.IO

import qualified Graphics.UI.SDL as SDL

data DisplayCommand =
    SolidColor SDL.Color SDL.Rect

paintCmd :: SDL.Renderer -> DisplayCommand -> IO ()
paintCmd re cmd = case cmd of
    SolidColor (SDL.Color r g b a) rect -> do
        ptrRect <- malloc
        poke ptrRect rect
        SDL.setRenderDrawColor re r g b a
        SDL.renderFillRect re ptrRect
        free ptrRect

paint :: SDL.Renderer -> [DisplayCommand] -> IO ()
paint re = mapM_ (paintCmd re)
