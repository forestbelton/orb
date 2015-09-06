module Layout.Types where

import Foreign.C.Types
import qualified Graphics.UI.SDL as SDL

data Edges = Edges {
    edgesTop :: CInt,
    edgesRight :: CInt,
    edgesBottom :: CInt,
    edgesLeft :: CInt
} deriving (Eq, Show)

data Layout = Layout {
    layoutContent :: SDL.Rect
--    padding :: Edges,
--    margin :: Edges,
--    border :: Edges
} deriving (Eq, Show)

noEdges :: Edges
noEdges = Edges 0 0 0 0

data BoxType = Block | Inline
    deriving (Eq, Show)

data NameType = Named | Anonymous
    deriving (Eq, Show)
