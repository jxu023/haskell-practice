module Queue where

import BinaryTreeNode

data Queue a = Queue [a] [a]

push :: Queue a -> a -> Queue a
push (Queue enq deq) x = Queue (x:enq) deq

pop :: Queue a -> (a, Queue a)
pop (Queue enq deq) | null deq  = let deq' = reverse enq
                                      in (head deq', Queue [] (tail deq'))
                    | otherwise = (head deq, Queue enq (tail deq))

empty :: Queue a -> Bool
empty (Queue enq deq) = null enq && null deq

emptyQ :: Queue a
emptyQ = Queue [] []

mkQ :: [a] -> Queue a
mkQ lst = Queue [] (reverse lst)

