module Layout where

import Debug.Trace

import DOM
import Paint
import Node
import Style
import qualified Style.Lookup as S
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL
import Foreign.C.Types

import Layout.Types
import Layout.Height

-- todo: make safer
findPx :: Style -> PropKey -> CInt
findPx (Style s) k = fromIntegral $ toPx $ M.findWithDefault (S.lookup (Style s) k) k s
    where toPx (NumUnit n Px) = n

-- todo: handle auto
computeWidth :: CInt -> Node (NodeType, BoxType, Style) -> CInt
computeWidth parentWidth (Node (_, Block, sty) _) = parentWidth - margin - padding - border
   where margin  = findPx sty MarginLeft + findPx sty MarginRight
         padding = findPx sty PaddingLeft + findPx sty PaddingRight
         border  = findPx sty BorderLeftWidth + findPx sty BorderRightWidth

buildLayout' :: Int -> Node (NodeType, BoxType, Style) -> Layout
buildLayout' y r@(Node (nt, Block, sty) cs) = n
    where n  = Node (nt, Dimensions (SDL.Rect 0 (fromIntegral y) w h), Block, sty) cs'
          w = 800
          h = fromIntegral $ y' - y
          (y', cs') = foldr go (y, []) (reverse cs) -- todo: figure out why i have to reverse here
          go c (y1, cs1) = (y1 + height c, buildLayout' y1 c : cs1)

buildLayout ::  Node (NodeType, BoxType, Style) -> Layout
buildLayout n = buildLayout' 0 n

buildDisplayCommands :: Layout -> [DisplayCommand]
buildDisplayCommands (Node (nodeTy, dim, Block, sty) cs) = dc : concatMap buildDisplayCommands cs
    where dc = case nodeTy of
                   Element _ _ -> displayRect (fromColor $ S.lookup sty BackgroundColor) (dimensionsContent dim)
                   Text s -> case dimensionsContent dim of
                       (SDL.Rect x y _ _ ) -> displayText x y (fromFont $ S.lookup sty FontFamily) (fromColor $ S.lookup sty FontColor) s

layout :: Node (NodeType, Style) -> [DisplayCommand]
layout = buildDisplayCommands . buildLayout . fmap (\(nt, sty) -> (nt, Block, sty))
