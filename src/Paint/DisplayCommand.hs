{-# LANGUAGE ExistentialQuantification #-}
module Paint.DisplayCommand where

import Data.IORef
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF.FFI (TTFFont)

type FontCache = M.Map String TTFFont
data SDLContext = SDLContext {
    contextWindow      :: SDL.Window,
    contextRenderer    :: SDL.Renderer,
    contextFontCache   :: IORef FontCache
 }

type DisplayCommand = SDLContext -> IO ()

displayNothing :: DisplayCommand
displayNothing ctx = return ()
