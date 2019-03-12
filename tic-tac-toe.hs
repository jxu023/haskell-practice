
import Data.Array

type Coord = (Int, Int)

newtype Board = Board (Array (Int, Int) Char)

instance Show Board where
    show = showBoard

allCells :: (Num a, Enum a) => [(a, a)]
allCells = [(i, j) | i <- [0..2], j <- [0..2]]

showBoard :: Board -> String
showBoard (Board b) =
    unlines (map (\r -> concat $ map (\c -> [' ', b ! (r, c)]) [sc..ec]) [sr..er])
    where ((sr, sc), (er, ec)) = bounds b

endp :: Board -> Coord -> Bool
endp (Board b) coord 
    = moved && (winner || tie)
      where moved = xo /= '_'
            winner = any (== 2) $ map (\dir@(x, y) -> sum $ map count
                                                                [((-1)*x, (-1)*y), dir])
                                      [(0, 1), (1, 1), (1, 0), (1, -1)]
            count diff = follow $ next coord
                where follow crd = if valid crd && b ! crd == xo
                                      then 1 + (follow (next crd))
                                      else 0
                      next (x, y) = (x + fst diff, y + snd diff)
            valid (x, y) = val x && val y where val x = x >= 0 && x < 3
            tie = all (== '_') [b ! cell | cell <- allCells]
            xo = b ! coord

emptyBoard :: Board
emptyBoard = Board $ array ((0,0), (2,2)) [(coord, '_') | coord <- allCells]

data Result = X_Win | O_Win | Tie
data Turn = X_Turn | O_Turn
data InPlay = InPlay
    { board :: Board
    , turn :: Turn
    }
data Game = InPlay | Result

main = do 
    -- print $ endp ["XOX", "OXX", "O_X"] [0, 0]
    print emptyBoard
    print $ endp emptyBoard (0,0)
