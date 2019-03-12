import Data.Array

newtype Board = Board (Array (Int, Int) Char)

instance Show Board where
    show = showBoard

allCells :: (Num a, Enum a) => [(a, a)]
allCells = [(i, j) | i <- [0..2], j <- [0..2]]

showBoard :: Board -> String
showBoard (Board b) =
    unlines (map (\r -> concat $ map (\c -> [' ', b ! (r, c)]) [sc..ec]) [sr..er])
    where ((sr, sc), (er, ec)) = bounds b

endp :: Board -> (Int, Int) -> Bool
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

--

data PlayState = PlayState
    { board :: Board
    , turn :: Turn
    }
data Turn = X_Turn | O_Turn

nextTurn :: Turn -> Turn
nextTurn X_Turn = O_Turn
nextTurn O_Turn = X_Turn

turnValue :: Turn -> Char
turnValue X_Turn = 'X'
turnValue O_Turn = 'O'

play :: PlayState -> (Int, Int) -> PlayState
play (PlayState (Board b) t) move
    = PlayState (Board (b // [(move, turnValue t)]))
                (nextTurn t)

-- next .. take user input .. need a function that takes an IO String
    -- extracts coordinate from IO String (getLine) and applies it to the game
    -- then calls getLine again until endp

-- TODO refactor endp to return Game instead of Bool, instaed of True returns one of Result data constructors
-- and instead of false would return the PlayState .. AHHH not really
-- endp should return some info about the result though .. doesn't have to be PlayState
-- create another datatype just for checking if it's terminal(?)
data Result = X_Win | O_Win | Tie
data Game = InPlay PlayState | Result
main = do 
    -- print $ endp ["XOX", "OXX", "O_X"] [0, 0] -- create test case handler
    print emptyBoard
    -- print $ endp emptyBoard (0,0)
    putStr "okay, let's start playing a game\n\n"
