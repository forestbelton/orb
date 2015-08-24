module Layout where

import Node
import Style

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

data BoxType = Block | Inline | Anonymous
type Layout = Node (Dimensions, BoxType, Style)

buildDisplayCommands :: Layout -> [DisplayCommand]
buildDisplayCommands = undefined
