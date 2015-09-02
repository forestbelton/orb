module Style (
      module Style.Types
    , module Style.Color
    , styleNode
) where

import DOM
import Node
import Style.Color
import Style.Parser
import Style.Types
import qualified Data.Map as M

styleNode :: DOMNode -> Node (NodeType, Style)
styleNode (Node nt cs) = Node (nt, sty) $ map styleNode cs
    where sty = case nt of
                    Element _ attrs -> maybe (Style M.empty) parseStyle $ M.lookup "style" attrs
                    _               -> Style M.empty
