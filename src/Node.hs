module Node (Node(..)) where

data Node a = Node a [Node a]
    deriving (Show, Eq)

instance Functor Node where
    fmap f (Node x cs) = Node (f x) (map (fmap f) cs)
