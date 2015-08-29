module Paint (DisplayCommand(..), paint, FontCache) where

import Control.Applicative
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign (with)

import qualified Data.Map as M
import Data.IORef
import System.IO

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.TTF.FFI (TTFFont)

data DisplayCommand
    = SolidColor SDL.Color SDL.Rect
    | FontData (CInt, CInt) String Int SDL.Color String

type FontCache = M.Map String TTFFont

getFont :: IORef FontCache -> String -> Int -> IO TTFFont
getFont cacheRef name weight = do
    cache <- readIORef cacheRef
    case M.lookup name cache of
        Just font  -> return font
        Nothing    -> do
            font <- TTF.openFont name weight
            writeIORef cacheRef (M.insert name font cache)
            return font

paintFont :: IORef FontCache -> SDL.Renderer -> CInt -> CInt -> String -> Int -> SDL.Color -> String -> IO ()
paintFont cacheRef re x y name weight color text = do
    font <- getFont cacheRef name weight
    textSurface <- TTF.renderUTF8Solid font text color
    textTexture <- SDL.createTextureFromSurface re textSurface
    textWidth <- SDL.surfaceW <$> peek textSurface
    textHeight <- SDL.surfaceH <$> peek textSurface
    alloca $ \ptrRect -> do
        poke ptrRect $ SDL.Rect x y textWidth textHeight
        SDL.renderCopy re textTexture nullPtr ptrRect
    return ()

paintRect :: SDL.Renderer -> SDL.Color -> SDL.Rect -> IO ()
paintRect re (SDL.Color r g b a) rect = do
    alloca $ \ptrRect -> do
        poke ptrRect rect
        SDL.setRenderDrawColor re r g b a
        SDL.renderFillRect re ptrRect
    return ()

paintCmd :: IORef FontCache -> SDL.Renderer -> DisplayCommand -> IO ()
paintCmd cacheRef re cmd = case cmd of
   SolidColor c rect -> do
        paintRect re c rect
   FontData (x, y) name weight color text ->
        paintFont cacheRef re x y name weight color text

paint :: IORef FontCache -> SDL.Renderer -> [DisplayCommand] -> IO ()
paint fc re = mapM_ (paintCmd fc re)
