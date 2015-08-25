module Layout where

import Paint
import Node
import Style
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL

data Edges = Edges {
    edgesTop :: Int,
    edgesRight :: Int,
    edgesBottom :: Int,
    edgesLeft :: Int
}

data Dimensions = Dimensions {
    dimensionsContent :: SDL.Rect,
    padding :: Edges,
    margin :: Edges,
    border :: Edges
}

noEdges :: Edges
noEdges = Edges 0 0 0 0

data BoxType = Block | Inline | Anonymous
type Layout = Node (Dimensions, BoxType, Style)

getBackgroundColor :: Style -> SDL.Color
getBackgroundColor (Style s) = maybe (SDL.Color 255 255 255 255) (\(Color c) -> c) $ M.lookup BackgroundColor s

buildDisplayCommands :: Layout -> [DisplayCommand]
buildDisplayCommands (Node (dim, Block, sty) cs) =
    SolidColor (getBackgroundColor sty) (dimensionsContent dim) : concatMap buildDisplayCommands cs
