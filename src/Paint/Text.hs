module Paint.Text (getFont, displayText) where

import Control.Applicative
import Data.IORef
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.TTF.FFI (TTFFont)

import Paint.DisplayCommand

getFont :: IORef FontCache -> String -> Int -> IO TTFFont
getFont cacheRef name weight = do
    cache <- readIORef cacheRef
    case M.lookup name cache of
        Just font  -> return font
        Nothing    -> do
            font <- TTF.openFont name weight
            writeIORef cacheRef (M.insert name font cache)
            return font

displayText :: CInt -> CInt -> TTFFont -> SDL.Color -> String -> DisplayCommand
displayText x y font col text ctx = do
    let cacheRef = contextFontCache ctx
    let re = contextRenderer ctx
--    font <- getFont cacheRef name weight
    textSurface <- TTF.renderUTF8Blended font text col
    textTexture <- SDL.createTextureFromSurface re textSurface
    textWidth <- SDL.surfaceW <$> peek textSurface
    textHeight <- SDL.surfaceH <$> peek textSurface
    alloca $ \ptrRect -> do
        poke ptrRect $ SDL.Rect x y textWidth textHeight
        SDL.renderCopy re textTexture nullPtr ptrRect
    SDL.destroyTexture textTexture
    SDL.freeSurface textSurface
