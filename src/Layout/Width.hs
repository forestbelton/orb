module Layout.Width (width) where

import Layout.Types

import DOM
import Node
import Style
import qualified Style.Lookup as S

import qualified Data.Map as M
import qualified Graphics.UI.SDL.TTF as TTF
import System.IO.Unsafe

width :: Int -> Node (NodeType, BoxType, Style) -> Int
width parentWidth (Node (nt, bt, Style sty) cs) = w
    where w = case nt of
                Text t      -> let Font font = S.lookup (Style sty) FontFamily in
                                 fst $ unsafePerformIO $ TTF.sizeUTF8 font t
                Element _ _ -> case bt of
                    Inline -> sum $ map (width parentWidth) cs
                    _  -> parentWidth
