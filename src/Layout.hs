module Layout where

import DOM
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
type Layout = Node (NodeType, Dimensions, BoxType, Style)

getBackgroundColor :: Style -> SDL.Color
getBackgroundColor (Style s) = maybe (SDL.Color 255 255 255 255) (\(Color c) -> c) $ M.lookup BackgroundColor s

-- todo: make safer
findPx :: Style -> PropKey -> CInt
findPx (Style s) k = fromIntegral $ toPx $ M.findWithDefault (defaults k) k s
    where toPx (Px n) = n

-- todo: handle auto
computeWidth :: CInt -> Node (NodeType, BoxType, Style) -> CInt
computeWidth parentWidth (Node (_, Block, sty) _) = parentWidth - margin - padding - border
   where margin  = findPx sty MarginLeft + findPx sty MarginRight
         padding = findPx sty PaddingLeft + findPx sty PaddingRight
         border  = findPx sty BorderLeftWidth + findPx sty BorderRightWidth

computeHeight :: Node (NodeType, BoxType, Style) -> CInt
computeHeight (Node (nt, _, sty) cs) = case nt of
    Element _ _ -> sum $ map computeHeight cs
    Text s -> 20 -- TODO: move into io monad so this works properly

buildLayout :: Node (NodeType, BoxType, Style) -> Layout
buildLayout n@(Node (nt, Block, sty) cs) = Node (nt, Dimensions (SDL.Rect 0 0 width height) paddingEdges marginEdges borderEdges, Block, sty) (map buildLayout cs)
    where width  = computeWidth 800 n
          height = computeHeight n
          paddingEdges = Edges (findPx sty PaddingTop) (findPx sty PaddingRight) (findPx sty PaddingBottom) (findPx sty PaddingLeft)
          marginEdges  = Edges (findPx sty MarginTop) (findPx sty MarginRight) (findPx sty MarginBottom) (findPx sty MarginLeft)
          borderEdges  = Edges (findPx sty BorderTopWidth) (findPx sty BorderRightWidth) (findPx sty BorderBottomWidth) (findPx sty BorderLeftWidth)

buildDisplayCommands :: Layout -> [DisplayCommand]
buildDisplayCommands (Node (nodeTy, dim, Block, sty) cs) = dc : concatMap buildDisplayCommands cs
    where dc = case nodeTy of
                   Element _ _ -> displayRect (getBackgroundColor sty) (dimensionsContent dim)
                   Text s -> case dimensionsContent dim of
                       (SDL.Rect x y _ _ ) -> displayText x y "./assets/arial.ttf" 12 (SDL.Color 0 0 0 255) s

layout :: Node (NodeType, Style) -> [DisplayCommand]
layout = buildDisplayCommands . buildLayout . fmap (\(nt, sty) -> (nt, Block, sty))
