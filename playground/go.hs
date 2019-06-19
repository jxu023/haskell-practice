import Data.Array

data GoCell = Empty | Black | White | Star
instance Show GoCell where
        show Black = "b"
        show White = "w"
        show Empty = "-"
        show Star  = "*"

other Black = White
other White = Black

type Coord = (Int, Int)

data GoBoard = GoBoard (Array Coord GoCell)

instance Show GoBoard where
        show (GoBoard arr) = unlines $ [showRow r | r <- [0..m]]
                where showRow r = unwords [show $ arr ! (r, c) | c <- [0..n]]
                      (m, n) = snd $ bounds arr

data GoGame = GoGame { goBoard :: GoBoard
                     , blackScore :: Int
                     , whiteScore :: Int
                     , koLocation :: Coord
                     }

mkBoard :: Int -> GoBoard
mkBoard sz = GoBoard $ array ((0, 0), (m, m)) [((i, j), Empty) | i <- [0..m], j <- [0..m]]
        where m = sz - 1

addStars :: GoBoard -> [Coord] -> GoBoard
addStars (GoBoard arr) lst = GoBoard $ arr // zip lst (repeat Star)

opTuple (a, b) (c, d) op = (op a c, op b d)
timesTuple a b = opTuple a b (*)
plusTuple a b = opTuple a b (+)

mk9Board :: GoBoard
mk9Board = addStars (mkBoard 9) [(4, 4)]
mk13Board = addStars (mkBoard 13) . zipWith plusTuple [(0, 0), (0, 12), (12, 0), (12, 12)]
                                  $ map (timesTuple (3, 3)) [(1, 1), (1, -1), (-1, 1), (-1, -1)]


main = putStrLn "hi" >> print mk13Board
