
import Chess

-- state, move, result type
class Game s m r where
    validMove :: s -> m -> Bool
    nextState :: s -> m -> s
    isOver :: s -> r

data ChessResult = Win Color | Tie

data ChessMove = ChessMove { src :: Coord
                           , dst :: Coord
                           }

instance Game ChessState ChessMove ChessResult where
    validMove = undefined
    nextState = undefined
    isOver = undefined

playGame :: (Game s m r) => IO r
playGame = undefined

seeGame :: (Game s m r) => s -> [m] -> s
seeGame = undefined

main = print "hi"
