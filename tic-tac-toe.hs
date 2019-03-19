import Data.Array
import Data.Char

allCells :: (Num a, Enum a) => [(a, a)]
allCells = [(i, j) | i <- [0..2], j <- [0..2]]

-- ******************************************************************

type Stone = Char
type Coord = (Int, Int)

newtype Board = Board (Array Coord Stone)

instance Show Board where
    show = showBoard

showBoard :: Board -> String
showBoard (Board b) =
    unlines (map (\r -> concat $ map (\c -> [' ', coordF b (r, c)])
                                     [sc..ec]) [sr..er])
    where ((sr, sc), (er, ec)) = bounds b -- s - start, r - row, e - end, c - col
          coordF b c = let v = b ! c
                         in if v == '_'
                              then intToDigit (c21 c)
                              else v

-- ******************************************************************

data GameStateResult = X_Win | O_Win | Tie | InPlay | InvalidMove

gameState :: Board -> Coord -> GameStateResult
gameState (Board b) coord 
  | xo == '_' = InPlay
  | winner    = if xo == 'X' then X_Win else O_Win
  | tie       = Tie
  | otherwise = InPlay
    where xo = b ! coord
          winner = any (== 2) $ map (\dir@(x, y) -> sum $ map count
                                                              [((-1)*x, (-1)*y), dir])
                                    [(0, 1), (1, 1), (1, 0), (1, -1)]
          count diff = follow $ next coord
            where follow crd = if valid crd && b ! crd == xo
                                  then 1 + (follow (next crd))
                                  else 0
                  next (x, y) = (x + fst diff, y + snd diff)
          valid (x, y) = val x && val y
          val x = x >= 0 && x < 3
          tie = all (== '_') [b ! cell | cell <- allCells]

-- ******************************************************************

emptyBoard :: Board
emptyBoard = Board $ array ((0,0), (2,2)) [(coord, '_') | coord <- allCells]

-- ******************************************************************

move :: Board -> Coord -> Stone -> Board
move (Board board) coord stone = Board (board // [(coord, stone)])

c12 :: Int -> (Int, Int)
c12 y = (div x 3, mod x 3) where x = y-1
c21 :: (Int,Int) -> Int
c21 (x, y) = x*3 + y + 1

validMove :: Board -> Coord -> Bool
validMove (Board b) c = b ! c == '_'

-- ******************************************************************

-- data PlayState = PlayState
--     { board :: Board
--     , turn :: Turn
--     }
-- data Turn = X_Turn | O_Turn

-- consider handler for parsing board states and test cases

-- data Game = InPlay PlayState | Result

-- consider combining gameState with move somehow ... seems more .. organic?
-- consider moving more logic into pure functions

-- ******************************************************************

-- .. converting everything to just a big  case statement ... that's basically like the same as a
-- c switch statement? ... kind of ? not really?
-- the cases should be at different "levels"
-- you could put them as all the same level .. but the function manipulating it should be at diff levels
-- only produce one certain condition from one certain founction
-- how do you reflect this in the definition though?


-- i think i kind of get it ...
-- maybe monad is useful for this 
-- but the way i am thinking here is a big switch table
-- 
-- the print thing .. don't need an assoc list specifically
-- just opverride the show instance for you GameResultType

main = do
  print emptyBoard
  go emptyBoard 'X'
  where go b stone
          = do c <- getChar
               putStrLn "\n"
               if c >= '1' && c <= '9'
                  then let coord = c12 (digitToInt c)
                           nb = move b coord stone
                           ns = if stone == 'X' then 'O' else 'X'
                       in if validMove b coord
                             then do print nb
                                     case gameState nb coord 
                                       of InPlay -> go nb ns
                                          X_Win -> print "X Wins!" -- print is repetitious
                                          O_Win -> print "O Wins!" -- refactor w/ assoc list
                                          Tie -> print "Tie Game!"
                                          InvalidMove -> do print "invalid move, try an empty cell"
                                                            print b
                                                            go b stone
                  else print "exiting"

-- ******************************************************************

-- the way to make the code "maintainable" may be to map a name onto as much as possible
-- then it might make things really customizable / easy to change ..
-- divide the code into "grabbable" parts (e.g. names)
