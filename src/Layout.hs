module Layout where

import Paint
import Node
import Style
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL
import Foreign.C.Types

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
type Layout = Node (Dimensions, BoxType, Style)

getBackgroundColor :: Style -> SDL.Color
getBackgroundColor (Style s) = maybe (SDL.Color 255 255 255 255) (\(Color c) -> c) $ M.lookup BackgroundColor s

-- todo: make safer
findPx :: Style -> PropKey -> CInt
findPx (Style s) k = fromIntegral $ toPx $ M.findWithDefault (defaults k) k s
    where toPx (Px n) = n

-- todo: handle auto
computeWidth :: CInt -> Node (BoxType, Style) -> CInt
computeWidth parentWidth (Node (Block, sty) _) = parentWidth - margin - padding - border
   where margin  = findPx sty MarginLeft + findPx sty MarginRight
         padding = findPx sty PaddingLeft + findPx sty PaddingRight
         border  = findPx sty BorderLeftWidth + findPx sty BorderRightWidth

computeHeight :: Node (BoxType, Style) -> CInt
computeHeight _ = 50

buildLayout :: Node (BoxType, Style) -> Layout
buildLayout n@(Node (Block, sty) cs) = Node (Dimensions (SDL.Rect 0 0 width height) paddingEdges marginEdges borderEdges, Block, sty) (map buildLayout cs)
    where width  = computeWidth 800 n
          height = computeHeight n
          paddingEdges = Edges (findPx sty PaddingTop) (findPx sty PaddingRight) (findPx sty PaddingBottom) (findPx sty PaddingLeft)
          marginEdges  = Edges (findPx sty MarginTop) (findPx sty MarginRight) (findPx sty MarginBottom) (findPx sty MarginLeft)
          borderEdges  = Edges (findPx sty BorderTopWidth) (findPx sty BorderRightWidth) (findPx sty BorderBottomWidth) (findPx sty BorderLeftWidth)

buildDisplayCommands :: Layout -> [DisplayCommand]
buildDisplayCommands (Node (dim, Block, sty) cs) =
    SolidColor (getBackgroundColor sty) (dimensionsContent dim) : concatMap buildDisplayCommands cs

layout :: Node Style -> [DisplayCommand]
layout = buildDisplayCommands . buildLayout . fmap (\sty -> (Block, sty))
