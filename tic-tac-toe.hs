import Data.Array
import Data.Char
-- import Debug.Trace
-- import the specific functions used, not the whole module

allCoords :: (Num a, Enum a) => [(a, a)]
allCoords = [(i, j) | i <- [0..2], j <- [0..2]]
-- refactor to use bounds? would need another param then

-- ******************************************************************
-- types denote computation ...
-- avoid higher level types until u see the pattern to factor out ..
-- then it becomes one for the toolbox
-- they should really define these things by solvable problems
-- i guess error handling is okay, but it really means control flow in general doesn't it
-- for monad?

type Turn = Char
type Coord = (Int, Int)

newtype Board = Board (Array Coord Turn)

instance Show Board where
    show = showBoard

showCell :: Array Coord Turn -> Coord -> Char
showCell arr coord
  | cell == '_' = intToDigit (c21 coord)
  | otherwise   = cell
  where cell = arr ! coord

showBoard :: Board -> String
showBoard (Board b) =
    unlines [concat [[' ', showCell b (r, c)] | c <- [sc..ec]] | r <- [sr..er]]
    where ((sr, sc), (er, ec)) = bounds b

-- ******************************************************************

data EndState = X_Win | O_Win | Tie | InvalidBoard deriving (Eq)

instance Show EndState where
    show X_Win = "X Wins!"
    show O_Win = "O Wins!"
    show Tie = "Tie Game!"
    show InvalidBoard = "Board is invalid!"

data InPlay = InPlay Board Turn
instance Show InPlay where
    show (InPlay b s) = show b

type GameState = Either EndState InPlay


-- consider change first input type to be GameState (?)
-- makes for simpler initialization (in main (?))
gameState :: InPlay -> Coord -> GameState
gameState (InPlay (Board b) turn) coord
  | not validMove = Right (InPlay (Board b) turn)
  | winner        = if turn == 'X' then Left X_Win else Left O_Win
  | tie           = Left Tie
  | otherwise     = Right (InPlay nextBoard (nextTurn turn))
  where winner = any (== 2) $ map (\(x, y) -> sum $ map count
                                                        [((-1)*x, (-1)*y), (x, y)])
                                  [(0, 1), (1, 1), (1, 0), (1, -1)]
        count diff = follow $ next coord
          where follow crd = if valid crd && b ! crd == turn
                                then 1 + (follow (next crd))
                                else 0
                next (x, y) = (x + fst diff, y + snd diff)
        validMove = valid coord && (b ! coord == '_')
        valid (x, y) = val x && val y
        val x = x >= 0 && x < 3
        tie = all (/= '_') [nextB ! cell | cell <- allCoords] -- was b instead of nextB ... should write a test for this somehow?
        nextB = b // [(coord, turn)]
        nextBoard = Board nextB

nextTurn :: Turn -> Turn
nextTurn turn = if turn == 'X' then 'O' else 'X'

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
  where xwin = swin 'X' X_Win
        owin = swin 'O' O_Win
        swin st win = s == st && any (== win) childrenEndStates
                      || s == nextTurn st && all (== win) childrenEndStates
        childrenEndStates = map solve [gameState ip coord | coord <- validCoords]
        validCoords = [coord | coord <- allCoords, b ! coord == '_']

-- if don't use swin , then i can also define the xwin condition like this:

-- (s == 'X' && any (== X_Win) nextStates)
-- || (s == 'O' && all (== X_Win) nextStates)

-- ******************************************************************

readBoard :: String -> Board
readBoard rows = Board $ array ((0,0), (2,2)) (go 1 rows)
  where go count [] = []
        go count (x:xs) = (c12 count, x):(go (count + 1) xs)

-- use monad notation since Maybe is a monad?
readState :: Maybe String -> GameState
readState (Just str) = Right (InPlay (readBoard (init str)) (last str))
readState Nothing = Left InvalidBoard

-- this could probably look nicer somehow?
-- i should be able to notify the reason for bad input
-- each one should be accompanied by a message
-- that would be a reason to use Either
verifyInputString :: String -> Maybe String
verifyInputString str
  = if valid then Just str else Nothing
    where valid = and [totalCount == 10,
                       noOtherCount,
                       xTurn || oTurn,
                       xTurn && (oCount == xCount)
                          || oTurn && (oCount - 1 == xCount)]
          totalCount = length str
          noOtherCount = null $ filter (\c -> and $ map (/= c) ['X', 'O', '_']) str
          xCount = length [c | c <- board, c == 'X']
          oCount = length [c | c <- board, c == 'O']
          xTurn = turn == 'X'
          oTurn = turn == 'O'
          board = init str
          turn = last str

readAndSolve :: String -> EndState
readAndSolve = solve . readState . verifyInputString

-- x to move then equal num stones
--

-- ******************************************************************

-- should also print final board, need to edit GameState
-- that way seems not as elegant as it is run now ... how to get the last board then?
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
                             Left endState  -> print endState 
                  else do print playState
                          print "exiting"

-- ******************************************************************

emptyBoard = Board $ array ((0,0), (2,2)) [(coord, '_') | coord <- allCoords]
initialState = Right (InPlay emptyBoard 'X')

-- ******************************************************************

xwins1 = "OXO\
         \XX_\
         \O__O"

tie1   = "___\
         \O__\
         \___X"

-- ******************************************************************
-- too much refactoring for incremental changes is a sign of having your functions
-- operate on types which are at the wrong level
-- if you find yourself unwrapping a variable by pattern matching ...
-- then perhaps you should probably just use a separate function

-- does the same thought apply for wrapping things?
-- should define a convention to wrap things only where you use the wrapped thing

-- verify input for readAndSolve ... seems board not right in prev test

-- store optimal sequence(s) of moves leading to solved result

-- refactor to list comprehensions .. is that better?

-- need to change the type of gamestate from Either a b to (MetaData, BoardState)
-- something like that ... what else to call it but metadata?

main = do
    print "running some testcases"
    print $ readAndSolve xwins1
    print $ readAndSolve tie1
