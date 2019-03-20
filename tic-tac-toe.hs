import Data.Array
import Data.Char

allCells :: (Num a, Enum a) => [(a, a)]
allCells = [(i, j) | i <- [0..2], j <- [0..2]] -- refactor this to use bounds .. see 16

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

data EndState = X_Win | O_Win | Tie
instance Show EndState where
    show X_Win = "X Wins!"
    show O_Win = "O Wins!"
    show Tie = "Tie Game!"

data InPlay = InPlay Board Stone
instance Show InPlay where
    show (InPlay b s) = show b

type GameState = Either EndState InPlay

gameState :: InPlay -> Coord -> GameState
gameState (InPlay (Board b) stone) coord
  | not validMove = Right (InPlay (Board b) stone)
  | winner        = if stone == 'X' then Left X_Win else Left O_Win
  | tie           = Left Tie
  | otherwise     = Right (InPlay nextBoard nextStone)
  where winner = any (== 2) $ map (\dir@(x, y) -> sum $ map count
                                                            [((-1)*x, (-1)*y), dir])
                                  [(0, 1), (1, 1), (1, 0), (1, -1)]
        count diff = follow $ next coord
          where follow crd = if valid crd && b ! crd == stone
                                then 1 + (follow (next crd))
                                else 0
                next (x, y) = (x + fst diff, y + snd diff)
        validMove = valid coord && (b ! coord == '_')
        valid (x, y) = val x && val y
        val x = x >= 0 && x < 3
        tie = all (/= '_') [b ! cell | cell <- allCells]
        nextBoard = Board (b // [(coord, stone)])
        nextStone = if stone == 'X' then 'O' else 'X'

-- ******************************************************************

c12 :: Int -> (Int, Int)
c12 y = (div x 3, mod x 3) where x = y-1
c21 :: (Int,Int) -> Int
c21 (x, y) = x*3 + y + 1

-- ******************************************************************

-- monad?
-- refactor w/o Either? https://www.reddit.com/r/haskell/comments/68upwd/how_to_easily_combine_types/
-- please improve your commit messages ... they're awful!

main = do
  let emptyBoard = Board $ array ((0,0), (2,2)) [(coord, '_') | coord <- allCells]
  print emptyBoard
  go (InPlay emptyBoard 'X')
  where go playState
          = do c <- getChar
               putStrLn "\n"
               if c >= '1' && c <= '9'
                  then case gameState playState (c12 (digitToInt c))
                          of Right nextPlay -> do print nextPlay -- a bit repetitious on prints again
                                                  go nextPlay    -- derive Show on GameState?
                             Left endState  -> print endState
                  else do print playState
                          print "exiting"

-- ******************************************************************

-- the way to make the code "maintainable" may be to map a name onto as much as possible
-- then it might make things really customizable / easy to change ..
-- divide the code into "grabbable" parts (e.g. names)
