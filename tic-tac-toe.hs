import Data.Array
import Data.Char

allCoords :: (Num a, Enum a) => [(a, a)]
allCoords = [(i, j) | i <- [0..2], j <- [0..2]]

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

data EndState = X_Win | O_Win | Tie deriving (Eq)
instance Show EndState where
    show X_Win = "X Wins!"
    show O_Win = "O Wins!"
    show Tie = "Tie Game!"

data InPlay = InPlay Board Stone
instance Show InPlay where
    show (InPlay b s) = show b

-- refactor to (GameResult, BoardState), remove Either
-- need this so can print out the final board
type GameState = Either EndState InPlay

-- consider change first input type to be GameState (?)
-- makes for simpler initialization (in main (?))
gameState :: InPlay -> Coord -> GameState
gameState (InPlay (Board b) stone) coord
  | not validMove = Right (InPlay (Board b) stone)
  | winner        = if stone == 'X' then Left X_Win else Left O_Win
  | tie           = Left Tie
  | otherwise     = Right (InPlay nextBoard (nextStone stone))
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
        tie = all (/= '_') [b ! cell | cell <- allCoords]
        nextBoard = Board (b // [(coord, stone)])

nextStone :: Stone -> Stone
nextStone stone = if stone == 'X' then 'O' else 'X'

-- ******************************************************************

c12 :: Int -> (Int, Int)
c12 y = (div x 3, mod x 3) where x = y-1
c21 :: (Int,Int) -> Int
c21 (x, y) = x*3 + y + 1

-- ******************************************************************

-- this needs to cache game states to be efficient
-- that means storing and doing a lookup
-- best if we could eliminate symmetrically equivalent states
solve :: GameState -> EndState
solve (Left end) = end
solve (Right ip@(InPlay (Board b) s))
  | xwin = X_Win
  | owin = O_Win
  | otherwise = Tie
  where tie = all (== Tie) nextStates
        xwin = swin 'X' X_Win
        owin = swin 'O' O_Win
        swin st win = s == st && all (== win) nextStates
                      || s == nextStone st && all (== win) nextStates
        nextStates = map solve [gameState ip coord | coord <- validCoords]
        validCoords = [coord | coord <- allCoords, b ! coord == '_']

-- ******************************************************************

readBoard :: String -> Board
readBoard rows = Board $ array ((0,0), (2,2)) (go 1 rows)
  where go count [] = []
        go count (x:xs) = (c12 count, x):(go (count + 1) xs)

readState :: String -> GameState
readState str = Right (InPlay (readBoard (init str)) (last str))

readAndSolve :: String -> EndState
readAndSolve = solve . readState

-- also add readInPlay, takes in the current stone as well
-- the real definition of read should just be string

-- ******************************************************************

playGame = do
  print emptyBoard
  go (InPlay emptyBoard 'X')
  where go playState
          = do c <- getChar
               putStrLn "\n"
               if c >= '1' && c <= '9'
                  then case gameState playState (c12 (digitToInt c))
                          of Right nextPlay -> do print nextPlay
                                                  go nextPlay
                             Left endState  -> print endState -- should also print final board, need to edit GameState
                                                              -- that way seems not as elegant as it is run now ... how to get the last board then?
                  else do print playState
                          print "exiting"

-- ******************************************************************

emptyBoard = Board $ array ((0,0), (2,2)) [(coord, '_') | coord <- allCoords]
initialState = Right (InPlay emptyBoard 'X')

-- ******************************************************************

xwins1 = "OXO\
         \XX_\
         \O__"

-- ******************************************************************

main = do
    print "running some testcases"
    print $ readAndSolve xwins1
