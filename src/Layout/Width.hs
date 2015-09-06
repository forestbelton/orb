{-# LANGUAGE FlexibleContexts #-}
module Layout.Width (width) where

import Layout.Types

import DOM
import Types
import Style
import qualified Style.Lookup as S

import Control.Lens hiding (children)
import qualified Graphics.UI.SDL.TTF as TTF
import System.IO.Unsafe

width :: (HasNodeType a NodeType, HasBoxType a BoxType, HasStyle a Style, HasChildren a [a]) => Int -> a -> Int
width parentWidth node = w
    where w = case node ^. nodeType of
                Text t      -> let Font font = S.lookup (node ^. style) FontFamily in
                                 fst $ unsafePerformIO $ TTF.sizeUTF8 font t
                Element _ _ -> case node ^. boxType of
                    Inline -> sum $ map (width parentWidth) (node ^. children)
                    _  -> parentWidth
