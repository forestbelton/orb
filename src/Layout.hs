module Layout where

import Node
import Style

data BoxType = Block | Inline | Anonymous
type Layout = Node (Dimensions, BoxType)

