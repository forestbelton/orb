module Paint (DisplayCommand(..), paint) where

import Control.Applicative
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign (with)

import System.IO

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

data DisplayCommand
    = SolidColor SDL.Color SDL.Rect
    | FontData (CInt, CInt) String Int SDL.Color String

paintCmd :: SDL.Renderer -> DisplayCommand -> IO ()
paintCmd re cmd = case cmd of
    SolidColor (SDL.Color r g b a) rect -> do
        alloca $ \ptrRect -> do
            poke ptrRect rect
            SDL.setRenderDrawColor re r g b a
            SDL.renderFillRect re ptrRect
            return ()
    FontData (x, y) name weight color text -> do
            font <- TTF.openFont name weight
            textSurface <- TTF.renderUTF8Solid font text color
            textTexture <- SDL.createTextureFromSurface re textSurface
            textWidth <- SDL.surfaceW <$> peek textSurface
            textHeight <- SDL.surfaceH <$> peek textSurface
            let loc = SDL.Rect x y textWidth textHeight
            with loc $ \loc' ->
             SDL.renderCopy re textTexture nullPtr loc'
            TTF.closeFont font


paint :: SDL.Renderer -> [DisplayCommand] -> IO ()
paint re = mapM_ (paintCmd re)
