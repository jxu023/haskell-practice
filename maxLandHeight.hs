
-- sealevel is 0
-- max ht diff 1 btwn neighs

import Data.Array
-- import Debug.Trace

type Arr = Array (Int, Int) Int
type Coord = (Int, Int)

mat1 :: [[Int]]
mat1 = [ [1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1],
         [1, 1, 0, 1, 1],
         [1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1],
         [1, 1, 1, 1, 1] ]

list2arr :: [[Int]] -> Arr
list2arr lst = array ((0, 0), (m, n)) (fill 0 0 lst)
        where m = (length lst) - 1
              n = (length $ head lst) - 1
              fill i j ((x:xs):rows) = ((i,j),x):(fill i (j + 1) (xs:rows))
              fill i j ([]:rows)     = fill (i + 1) 0 rows
              fill _ _ []            = []

-- mark array cells as 0 to indicate visited
maxHeight :: Arr -> Int
maxHeight = bfs . sea_level
        where sea_level arr = (0, sea_cells, [], arr)
                      where sea_cells = map fst . filter ((== 0) . snd) $ assocs arr
              bfs (height, [], [], _)          = height
              bfs (height, [], next, arr)      = bfs (height + 1, next, [], arr)
              bfs (height, (x:cur), next, arr) = bfs (height, cur, next', arr // visited)
                      where next' = adj x ++ next
                            visited = zip (adj x) (repeat 0)
                            adj (i,j) = filter valid [(i + di, j + dj) | (di, dj) <- [(-1, 0), (1, 0), (0, 1), (0, -1)]]
                            valid x' = and [op (dim x') (dim lim) | dim       <- [fst, snd],
                                                                    (op, lim) <- [((>=), fst bnd), ((<=), snd bnd)]]
                                       && arr ! x' > 0
                            bnd = bounds arr

-- try printing out that intermediate array .. use trace
main = do
        print . show . maxHeight $ list2arr mat1
        print "hi"
