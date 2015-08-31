module Layout.Height (height) where

import Layout.Types

import DOM
import Node
import Style
import qualified Style.Lookup as S

import qualified Data.Map as M
import qualified Graphics.UI.SDL.TTF as TTF
import System.IO.Unsafe

height :: Node (NodeType, BoxType, Style) -> Int
height (Node (nt, _, Style sty) cs) = h + (sum $ map height cs)
    where h = case nt of
                Text t      -> let Font font = S.lookup (Style sty) FontFamily in
                                 snd $ unsafePerformIO $ TTF.sizeUTF8 font t
                Element _ _ -> (\(Px n) -> n) $ M.findWithDefault (Px 0) Height sty
