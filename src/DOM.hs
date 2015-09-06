module DOM where

import Types

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import qualified Data.Map as M

stripEmptyText :: [Tag String] -> [Tag String]
stripEmptyText = filter justWS
    where justWS (TagText s) = not $ all (\c -> c == ' ' || c == '\r' || c == '\n') s
          justWS x           = True

parseDOM :: String -> DomNode
parseDOM = go . head . tagTree . stripEmptyText . parseTags
    where go (TagBranch name attrs cs) = DomNode (Element name $ M.fromAscList attrs) (map go cs)
          go (TagLeaf (TagText t))     = DomNode (Text t) []
