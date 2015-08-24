module Style where

import qualified Data.Map as M
import Text.CSS.Parse

data PropKey
    = MarginLeft
    | BorderLeftWidth
    | PaddingLeft
    | Width
    | PaddingRight
    | BorderRightWidth
    | MarginRight
    | BackgroundColor

data PropVal
    = Px Int
    | Color Int Int Int
    | Auto

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

data Style = M.Map PropKey PropVal
