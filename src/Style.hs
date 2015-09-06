module Style (
      module Style.Types
    , module Style.Color
    , styleNode
) where

import Types
import DOM
import Style.Color
import Style.Parser
import Style.Types

import Control.Lens hiding (children)
import qualified Data.Map as M

styleNode :: DomNode -> StyledNode
styleNode node = StyledNode nt sty $ map styleNode $ node ^. children
    where nt = node ^. nodeType
          sty = case nt of
                    Element _ attrs -> maybe newStyle parseStyle $ M.lookup "style" attrs
                    _               -> newStyle
