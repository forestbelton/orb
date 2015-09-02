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
} deriving (Show)

data Dimensions = Dimensions {
    dimensionsContent :: SDL.Rect
--    padding :: Edges,
--    margin :: Edges,
--    border :: Edges
} deriving (Show)

noEdges :: Edges
noEdges = Edges 0 0 0 0

data BoxType = Block | Inline | Anonymous deriving (Show)
type Layout = Node (NodeType, Dimensions, BoxType, Style)
