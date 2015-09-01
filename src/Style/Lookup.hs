module Style.Lookup (lookup) where

import Style
import Style.Color
import Style.Types

import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.TTF.FFI (TTFFont)
import System.IO.Unsafe

lookup :: Style -> PropKey -> PropVal
lookup (Style sty) key = M.findWithDefault (defaults key) key sty

defaultFont :: TTFFont
{-# NOINLINE defaultFont #-}
defaultFont = unsafePerformIO $ TTF.init >> TTF.openFont "./assets/arial.ttf" 12

defaults :: PropKey -> PropVal
defaults FontColor       = black
defaults FontFamily      = Font defaultFont
defaults BackgroundColor = white
defaults _               = NumUnit 0 Px
