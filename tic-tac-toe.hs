
import Data.Array

zipPlus coord diff = zipWith (+) coord diff

type Coord = (Int, Int)

get :: Board -> Coord -> Char
get b c = (!)

endp :: Board -> Coord -> Bool
endp board coord 
    = moved && (winner || tie)
      where moved = xo /= '_'
            winner = any (== 2) $ map (\dir -> sum $ map count [map (* (-1)) dir, dir])
                                      [[0, 1], [1, 1], [1, 0], [1, -1]]
            count diff = follow $ next coord
                where follow crd = if valid crd && get board crd == xo
                                      then (+ 1) . follow $ next crd
                                      else 0
                      next x = zipPlus x diff
            valid coord = and [pred dim | dim <- coord, pred <- [(>= 0), (< 3)]]
            tie = (not . any (== '_') $ concat board)
            xo = get board coord

data Result = X_Win | O_win | Tie

-- this to monads .. doesn't have to be explicit really though
data Game = InPlay | Result

newtype Board = Board (Array (Integer, Integer) Char)

showBoard :: Board -> String
showBoard (Board b) =
    unlines (map (\r -> concat $ map (\c -> [' ', b ! (r, c)]) [sc..ec]) [sr..er])
    where ((sr, sc), (er, ec)) = bounds b

instance Show Board where
    show = showBoard

allCells :: (Num a, Enum a) => [(a, a)]
allCells = [(i, j) | i <- [0..2], j <- [0..2]]

emptyBoard :: Board
emptyBoard = Board $ array ((0,0), (2,2)) [(coord, '_') | coord <- allCells]

main = do 
    print $ endp ["XOX", "OXX", "O_X"] [0, 0]
    print emptyBoard
