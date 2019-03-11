
-- get board coord = board !! (coord !! 0) !! (coord !! 1)

data Matrix a = Cell a | Nested (Matrix [a])

get :: Num a => Matrix a -> [a] -> a
get (Nested (Nested space)) (dim:dims) = if null dims then val else get val dims where val = space !! dim

main = do
  print "hi"

