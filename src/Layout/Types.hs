module Layout.Types where

import DOM
import Node
import Style

import Foreign.C.Types
import qualified Graphics.UI.SDL as SDL

data Edges = Edges {
    edgesTop :: CInt,
    edgesRight :: CInt,
    edgesBottom :: CInt,
    edgesLeft :: CInt
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
type Layout = Node (NodeType, Dimensions, BoxType, Style)
