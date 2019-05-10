import Data.Array
import Data.Char

c12 :: Int -> (Int, Int)
c12 y = (div x 3, mod x 3) where x = y-1
c21 :: (Int,Int) -> Int
c21 (x, y) = x*3 + y + 1

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

allCoords :: (Num a, Enum a) => [(a, a)]
allCoords = [(i, j) | i <- [0..2], j <- [0..2]]

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
        tie = all (/= '_') [nextB ! cell | cell <- allCoords]
        nextB = b // [(coord, turn)]
        nextBoard = Board nextB

nextTurn :: Turn -> Turn
nextTurn turn = if turn == 'X' then 'O' else 'X'

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

readBoard :: String -> Board
readBoard rows = Board $ array ((0,0), (2,2)) (go 1 rows)
  where go count [] = []
        go count (x:xs) = (c12 count, x):(go (count + 1) xs)

readState :: Maybe String -> GameState
readState (Just str) = Right (InPlay (readBoard (init str)) (last str))
readState Nothing = Left InvalidBoard

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

emptyBoard = Board $ array ((0,0), (2,2)) [(coord, '_') | coord <- allCoords]
initialState = Right (InPlay emptyBoard 'X')

testCases = [("OXO\
              \XX_\
              \O__O", X_Win),
             ("___\
              \O__\
              \___X", Tie),]

main = do
    print "Let's play some TicTacToe."
