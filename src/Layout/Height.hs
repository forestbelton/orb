{-# LANGUAGE FlexibleContexts #-}
module Layout.Height (height) where

import Layout.Types

import DOM
import Types
import Style
import qualified Style.Lookup as S

import Control.Lens hiding (children)
import qualified Graphics.UI.SDL.TTF as TTF
import System.IO.Unsafe

height :: (HasNodeType a NodeType, HasStyle a Style, HasChildren a [a]) => a -> Int
height node = h + (sum $ map height $ node ^. children)
    where sty = node ^. style
          h = case node ^. nodeType of
                Text t      -> let Font font = S.lookup sty FontFamily in
                                 snd $ unsafePerformIO $ TTF.sizeUTF8 font t
                Element _ _ -> fromPx $ S.lookup sty Height
