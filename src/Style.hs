module Style where

import DOM
import Node

import Graphics.UI.SDL.TTF.FFI (TTFFont)
import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as M
import Text.CSS.Parse
import Data.Text (pack, unpack)

data PropKey
    = MarginTop
    | MarginRight
    | MarginBottom
    | MarginLeft
    | Width
    | BorderTopWidth
    | BorderRightWidth
    | BorderLeftWidth
    | BorderBottomWidth
    | BackgroundColor
    | PaddingTop
    | PaddingRight
    | PaddingBottom
    | PaddingLeft
    | Height
    | FontFamily
  deriving (Eq, Ord, Show)

data PropVal
    = Px Int
    | Color SDL.Color
    | Auto
    | Font TTFFont
  deriving (Eq, Show)

-- TODO: Replace with real parser
keyStr :: String -> PropKey
keyStr "background-color" = BackgroundColor
keyStr "width" = Width

valStr :: String -> PropVal
valStr "red" = Color (SDL.Color 255 0 0 255)
valStr "green" = Color (SDL.Color 0 255 0 255)
valStr "blue" = Color (SDL.Color 0 0 255 255)
valStr s | reverse (take 2 (reverse s)) == "px" = Px (read $ reverse $ drop 2 $ reverse s)

newtype Style = Style (M.Map PropKey PropVal)
  deriving (Show)

-- ultimately should look like this:
--
-- M.fromAscList $ catMaybes $ map (\(k, v) -> (,) <$> propKey k <*> propVal v)

parseStyle :: String -> (String, Style)
parseStyle s = case parseBlock $ pack s of
    Right (sel, props) -> (unpack sel, Style $ M.fromAscList $ map (\(k, v) -> (keyStr (unpack k), valStr (unpack v))) props)

parseStyle' :: String -> Style
parseStyle' s = case parseAttrs $ pack s of
    Right props -> Style $ M.fromAscList $ map (\(k, v) -> (keyStr (unpack k), valStr (unpack v))) props

styleNode :: DOMNode -> Node (NodeType, Style)
styleNode (Node nt cs) = Node (nt, sty) $ map styleNode cs
    where sty = case nt of
                    Element _ attrs -> maybe (Style M.empty) parseStyle' $ M.lookup "style" attrs
                    _               -> Style M.empty
