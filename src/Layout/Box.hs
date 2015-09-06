module Layout.Box (makeBoxModel, makeAnonymousBox) where

import Control.Lens hiding (children)
import Data.Maybe

import DOM
import Types
import Layout.Types
import Style.Types
import qualified Style.Lookup as S

displayToBox :: DisplayType -> Maybe BoxType
displayToBox NoneType   = Nothing
displayToBox InlineType = Just Inline
displayToBox BlockType  = Just Block

makeBoxModel :: StyledNode -> [BoxNode]
makeBoxModel node = maybeToList . fmap go . displayToBox . fromDisplayVal $ S.lookup sty Display
    where sty = node ^. style
          go bt = BoxNode (node ^. nodeType) sty Named bt cs
          cs = concatMap makeBoxModel (node ^. children)

-- hack for now, we really need to make NodeType optional on BoxNode
makeAnonymousBox :: [BoxNode] -> BoxNode
makeAnonymousBox = BoxNode (Text " ") newStyle Anonymous Block
