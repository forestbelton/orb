module Style where

import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as M
import Text.CSS.Parse
import Data.Text (pack, unpack)

data PropKey
    = MarginLeft
    | BorderLeftWidth
    | PaddingLeft
    | Width
    | PaddingRight
    | BorderRightWidth
    | MarginRight
    | BackgroundColor
  deriving (Eq, Ord, Show)

data PropVal
    = Px Int
    | Color SDL.Color
    | Auto
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

defaults :: PropKey -> PropVal
defaults k = maybe (error "unknown property") id $ M.lookup k vals
    where vals = M.fromAscList [
                    (MarginLeft, Px 0)
                  , (BorderLeftWidth, Px 0) -- TODO: replace with medium
                  , (PaddingLeft, Px 0)
                  , (Width, Auto)
                  , (PaddingRight, Px 0)
                  , (BorderRightWidth, Px 0) -- TODO: replace with medium
                  , (MarginRight, Px 0)
                ]

newtype Style = Style (M.Map PropKey PropVal)
  deriving (Show)

-- ultimately should look like this:
--
-- M.fromAscList $ catMaybes $ map (\(k, v) -> (,) <$> propKey k <*> propVal v)

parseStyle :: String -> (String, Style)
parseStyle s = case parseBlock $ pack s of
    Right (sel, props) -> (unpack sel, Style $ M.fromAscList $ map (\(k, v) -> (keyStr (unpack k), valStr (unpack v))) props)
