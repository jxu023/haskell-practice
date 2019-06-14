
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

mk9Board :: GoBoard
mk9Board = addStars (mkBoard 9) [(4, 4)]

main = putStrLn "hi" >> print mk9Board
