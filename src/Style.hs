module Style (
      module Style.Types
    , styleNode
) where

import DOM
import Node
import Style.Types

import Graphics.UI.SDL.TTF.FFI (TTFFont)
import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as M
import Text.CSS.Parse
import Data.Text (pack, unpack)

-- TODO: Replace with real parser
keyStr :: String -> PropKey
keyStr "color"            = FontColor
keyStr "background-color" = BackgroundColor
keyStr "width"            = Width
keyStr "height"           = Height

valStr :: String -> PropVal
valStr "red"   = Color (SDL.Color 255 0 0 255)
valStr "green" = Color (SDL.Color 0 255 0 255)
valStr "blue"  = Color (SDL.Color 0 0 255 255)
valStr "white" = Color (SDL.Color 255 255 255 255)
valStr s | reverse (take 2 (reverse s)) == "px" = Px (read $ reverse $ drop 2 $ reverse s)

-- ultimately should look like this:
--
-- M.fromAscList $ catMaybes $ map (\(k, v) -> (,) <$> propKey k <*> propVal v)

parseStyle :: String -> Style
parseStyle s = case parseAttrs $ pack s of
    Right props -> Style $ M.fromAscList $ map (\(k, v) -> (keyStr (unpack k), valStr (unpack v))) props

styleNode :: DOMNode -> Node (NodeType, Style)
styleNode (Node nt cs) = Node (nt, sty) $ map styleNode cs
    where sty = case nt of
                    Element _ attrs -> maybe (Style M.empty) parseStyle $ M.lookup "style" attrs
                    _               -> Style M.empty
