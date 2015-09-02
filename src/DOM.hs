module DOM where

import Node

import qualified Data.Text as T
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import qualified Data.Map as M

type DOMNode = Node NodeType

data NodeType
    = Text String
    | Element String (M.Map String String)
    | Comment String
    deriving (Show)

text :: String -> DOMNode
text str = Node (Text str) []

elmt :: String -> M.Map String String -> [DOMNode] -> DOMNode
elmt tagName attrMap = Node (Element tagName attrMap)

onDOMNodeType :: (NodeType -> a) -> DOMNode -> a
onDOMNodeType f (Node nt _) = f nt

nodeName :: DOMNode -> String
nodeName = onDOMNodeType go
    where go (Text _)            = "#text"
          go (Element tagName _) = tagName
          go (Comment _)         = "#comment"

stripEmptyText :: [Tag String] -> [Tag String]
stripEmptyText = filter justWS
    where justWS (TagText s) = not $ all (\c -> c == ' ' || c == '\r' || c == '\n') s
          justWS x           = True

parseDOM :: String -> DOMNode
parseDOM = go . head . tagTree . stripEmptyText . parseTags
    where go (TagBranch name attrs cs) = Node (Element name $ M.fromAscList attrs) (map go cs)
          go (TagLeaf (TagText t))     = Node (Text t) []
