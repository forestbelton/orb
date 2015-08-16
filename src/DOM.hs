module DOM where

import Text.HTML.TagSoup
import qualified Data.Map as M

data Node = Node NodeType [Node]
    deriving (Show)

data NodeType
    = Text String
    | Element String (M.Map String String)
    | Comment String
    deriving (Show)

text :: String -> Node
text str = Node (Text str) []

elmt :: String -> M.Map String String -> [Node] -> Node
elmt tagName attrMap = Node (Element tagName attrMap)

onNodeType :: (NodeType -> a) -> Node -> a
onNodeType f (Node nt _) = f nt

nodeName :: Node -> String
nodeName = onNodeType go
    where go (Text _)            = "#text"
          go (Element tagName _) = tagName
          go (Comment _)         = "#comment"

parseOpenTag :: String -> [(String, String)] -> [Node] -> [Tag String] -> (Node, [Tag String])
parseOpenTag name attrs ts []      = error $ "unclosed open tag <" ++ name ++ ">"
parseOpenTag name attrs ts (h : t) = case h of
    TagText s -> parseOpenTag name attrs (text s : ts) t
    TagOpen n1 attrs1 -> let (n, us) = parseOpenTag n1 attrs1 [] t in
        parseOpenTag name attrs (n : ts) us
    TagClose name1 ->
        if name == name1
            then (elmt name (M.fromAscList attrs) ts, t)
            else error $ "found stray closing tag </" ++ name1 ++ ">"

parse :: String -> [Node]
parse s = parse' (parseTags s) []
    where parse' [] nodes     = nodes
          parse' (h : t) nodes = case h of
              TagText s -> parse' t (text s : nodes)
              TagOpen name attrs -> let (n, us) = parseOpenTag name attrs [] t in
                  parse' us (n : nodes)
              TagClose name -> error $ "found stray closing tag </" ++ name ++ ">"
