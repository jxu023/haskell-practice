
module BinaryTreeNode where 

data Node a = Empty | Node { value :: a,  left :: Node a , right :: Node a }
            deriving (Show)
