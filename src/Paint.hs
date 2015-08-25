module Paint (DisplayCommand(..), paint) where

import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign (with)

import System.IO

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF


data DisplayCommand =
    SolidColor SDL.Color SDL.Rect | FontData (Int, Int) String Int SDL.Color String

arial :: String
arial = "../assets/arial.ttf"

paintCmd :: SDL.Renderer -> DisplayCommand -> IO ()
paintCmd re cmd = case cmd of
    SolidColor (SDL.Color r g b a) rect -> do
        alloca $ \ptrRect -> do
            poke ptrRect rect
            SDL.setRenderDrawColor re r g b a
            SDL.renderFillRect re ptrRect
            return ()
    FontData (x, y) name weight color text -> do
         TTF.withInit $ do
            font <- TTF.openFont arial 150
            textSurface <- TTF.renderUTF8Solid font "test" (SDL.Color 255 255 255 1)
            textTexture <- SDL.createTextureFromSurface re textSurface
            let loc = SDL.Rect 320 240 150 100
            with loc $ \loc' ->
             SDL.renderCopy re textTexture nullPtr loc'
            return ()
     

paint :: SDL.Renderer -> [DisplayCommand] -> IO ()
paint re = mapM_ (paintCmd re)
