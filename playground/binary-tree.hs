import Queue
import BinaryTreeNode
import Debug.Trace

tryPushNodes :: Queue (b, Node a) -> [(b, Node a)] -> Queue (b, Node a)
tryPushNodes q lst = foldr tryPush q (reverse lst)
        where tryPush (_, Empty) = id
              tryPush node  = flip push node

heapIndex :: Show a => Node a -> [(Int, String)]
heapIndex root = go (mkQ [(1, root)]) emptyQ
        where go cur next | empty cur && empty next = []
                          | empty cur               = go next emptyQ
                          | otherwise               = (ind, str):(go cur' next')
                      where ((ind, node), cur') = pop cur
                            str = show . value $ node
                            next' = tryPushNodes next [(2*ind, left node), ((2*ind + 1), right node)]

-- bfs :: Queue a -> Queue a -> [b]
-- bfs initLayer pushp outf acc init = go 
-- bfs cur next out pushp outf acc init

-- showTreeRange :: Show a => Node a -> Int -> Int -> String
showTreeRange root treeWidth slotLength = bfs (mkQ [((1, treeWidth), root)]) emptyQ []
-- factor out bfs
        where bfs cur next out
                      | empty cur && empty next = flush
                      | empty cur               = flush ++ (bfs next emptyQ [])
                      | otherwise               = bfs cur' next' out'
                where (((lo, hi), node), cur') = pop cur
                      mid = div (hi + lo) 2
                      next' | hi == lo = next
                            | otherwise = tryPushNodes next [((lo, mid - 1), left node),
                                                             ((mid + 1, hi), right node)]

                      out' = out ++ front ++ slot
                      front = spaces $ slotLength * (mid - 1) - length out

-- factor this out ... space slot or osmething
                      slot = buffer ++ spaces (slotLength - length buffer)

                      buffer = slack ++ content ++ slack
                      slack = spaces (div (slotLength - length content) 2)
                      content = show (value node)

                      flush = out ++ back ++ "\n"
                      back = spaces $ treeWidth * slotLength - length out
              spaces n = take n $ repeat '|'

height :: Node a -> Int
height Empty = 0
height (Node v l r) = 1 + max (height l) (height r)

maxSlot :: Show a => Node a -> Int
maxSlot Empty = 0
maxSlot (Node v l r) = max (length $ show v) (max (maxSlot l) (maxSlot r))

prettyPrint :: Show a => Node a -> IO ()
prettyPrint node = putStr $ showTreeRange node (2 ^ height node - 1) (2 + maxSlot node) ++ "\n"

-- randomly generate a binary tree, we could generate a random number
-- and then map integers to binary trees

-- existence function for and indexed node in complete binary tree
-- log(n)^2 node count function of complete binary tree 
-- how about a binary tree that maintains the original set's order?
-- balanced stuff
-- look at data.sequence

main = do
        print "hello world"
        prettyPrint (Node 1 Empty Empty)
        prettyPrint (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty))
        prettyPrint (Node 1 (Node 2 Empty (Node 3 Empty Empty)) Empty)
        prettyPrint (Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) Empty)
        prettyPrint (Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty (Node 5 Empty Empty))) Empty)

