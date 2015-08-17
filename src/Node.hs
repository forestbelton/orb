module Node (Node(..)) where

data Node a = Node a [Node a]
    deriving (Show, Eq)
