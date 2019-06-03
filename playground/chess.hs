import Data.Array
import Data.Char
import Data.List

{-
- TODOs
-
- king check rules
- input notation parsing
- chat bot integration
- unit tests
-
- create your own notation e.g. e2 to e4
- let the game accept either notation
-
- prompt moves w/ algebraic notation e.g. Ne4
- maintain a data structure containing location of every piece
-    data PieceStatus = Dead | Live Coord
- allows for faster retrieval
- probably keep separate from gamestate
- just maintain it side by side during game execution
-
- divide into modules/files
-   ChessTypes.hs
-   ChessChars.hs
-   ChessRules.hs
-   ChessGame.hs
- refer to ~/rwh/ examples
-
- consider alterantive ways to express game rules or arbitrary constraints
- some kind of dsl? look up logic programming
- essentially it should just eliminate a whole bunch of possible rules
-   could do something like apply a filter onto certain "move types"
-   e.g. king cannot castle while in check, filter 2-space king moves
-
-   kind of an odd way to characterize
-   should write out the rules in natural language first and then write code
-   it'll make the code more readable in that fashion
-   literate programming eh?
-
- look into ben lynn's haskell js gui
-}

type Coord = (Int, Int)

data Color = Black | White deriving (Eq)
instance Show Color where
    show Black = "Black"
    show White = "White"

data Role = Pawn | Rook | Knight | Bishop | King | Queen
          deriving (Eq)

data ChessCell = Empty
               | Piece {refColor :: Color, refRole :: Role}
               | OutOfBounds
               deriving (Eq)

type ChessBoard = Array Coord ChessCell

data ChessState = ChessState { refBoard :: ChessBoard
                             , refEnpassant :: (Coord, Coord)
                             , refCastleWhiteKside :: Bool
                             , refCastleWhiteQside :: Bool
                             , refCastleBlackKside :: Bool
                             , refCastleBlackQside :: Bool
                             , refTurn :: Color
                             }

-- TODO consider polymorphism over switch/if/case
-- ChessMove typeclass ... or look up other OO techs in haskell
whiteChar role = case role of
                        Pawn -> '♙'
                        Rook -> '♖'
                        Knight -> '♘'
                        Bishop -> '♗'
                        King -> '♔'
                        Queen -> '♕'

blackChar role = case role of
                        Pawn -> '♟'
                        Rook -> '♜'
                        Knight -> '♞'
                        Bishop -> '♝'
                        King -> '♚'
                        Queen -> '♛'

pieceChar :: ChessCell -> Char
pieceChar (Piece Black role) = blackChar role
pieceChar (Piece White role) = whiteChar role
pieceChar Empty = ' '

interleave :: String -> String -> String
interleave _ [] = []
interleave [] _ = []
interleave (a:as) (b:bs) = a:b:(interleave as bs)

coordRow :: String
coordRow = " " ++ interleave (repeat ' ') ['a'..'h']

boardRow :: Int -> [Char] -> String
boardRow r cells = r' ++ interleave (repeat '|') (cells ++ r')
    where r' = show $ 8 - r

rowVals :: Array Coord ChessCell -> Int -> String
rowVals b r = [pieceChar $ b ! (r, c) | c <- [0..7]]

boardGrid :: Array Coord ChessCell -> [String]
boardGrid b = [boardRow r (rowVals b r) | r <- [0..7]]

instance Show ChessState where
        show (ChessState b p wk wq bk bq turn)
                = unlines $ coordRow:(boardGrid b) ++
                [coordRow,
                 show turn ++ " to move",
                 "castle " ++ show bq ++ " " ++ show bk,
                 "castle " ++ show wq ++ " " ++ show wk,
                 "Enpassant " ++ show p]

outBounds (dr, dc) = dr < 0 || dc < 0 || dr > 7 || dc > 7
inBounds = not . outBounds

at board coord | inBounds coord = board ! coord
               | otherwise      = OutOfBounds

keepTrue :: [(Bool, a)] -> [a]
keepTrue [] = []
keepTrue ((bool, x):lst) | bool = x:(keepTrue lst)
                         | otherwise = keepTrue lst

otherColor :: Color -> Color
otherColor Black = White
otherColor White = Black

movePiece :: ChessState -> (Coord, Coord) -> ChessState
movePiece state@(ChessState board passant wk wq bk bq turn) (src, dst)
        = state { refBoard = board'
                , refEnpassant = passant'
                , refCastleWhiteKside = wk'
                , refCastleWhiteQside = wq'
                , refCastleBlackKside = bk'
                , refCastleBlackQside = bq'
                , refTurn = otherColor turn
                }
        where board = refBoard state
              srcPiece = at board src
              board' = board // [(src, Empty), (dst, srcPiece)]
                                 -- ++ keepTrue [(castlep, (rookDst, rookPiece)),
                                 --              (castlep, (rookSrc, Empty)),
                                 --              (passantp, (passantSrc, Empty))]
              passant' = passant
              wk' = wk
              wq' = wq
              bk' = bk
              bq' = bq

horiz :: [Coord]
horiz = [(0, 1), (0, -1)]
vert = [(1, 0), (-1, 0)]
diag = [(i, j) | i <- [-1, 1], j <- [-1, 1]]
eightDirs = concat [horiz, vert, diag]
ljump = [(i, j) | i <- [-1, 1, 2, -2], j <- [-1, 1, 2, -2], abs i /= abs j]

plusTuple (r, c) (dr, dc) = (r + dr, c + dc)

-- characterizes piece movement
moves :: ChessState -> Coord -> [Coord]
moves (ChessState board passant castleWK castleWQ castleBK castleBQ turn) src =
        let piecep coord = case at board coord of Piece _ _ -> True
                                                  _ -> False
            whitep coord = case at board coord of Piece White _ -> True
                                                  _ -> False
            blackp coord = case at board coord of Piece Black _ -> True
                                                  _ -> False
            emptyp coord = case at board coord of Empty -> True
                                                  _ -> False
            sameColor dst = piecep src && piecep dst && whitep src == whitep dst
            diffColor dst = piecep dst && piecep src && not (sameColor dst)
            pawnrowp coord@(r, _) = r == 1 && blackp coord || r == 6 && whitep coord
            passantrowp coord@(r, _) = r == 2 && whitep coord || r == 5 && blackp coord
            -- pawn movement
            pawnFwd dir = let dst = plusTuple src dir
                              dst2 = plusTuple dst dir
                          in keepTrue [(emptyp dst, dst),
                                       (emptyp dst && emptyp dst2 && pawnrowp src, dst2)]
            pawnTake dirs = dirs >>= \dir ->
                    let dst = plusTuple src dir
                    in keepTrue [(diffColor dst || fst passant == dst && passantrowp src, dst)]
            -- non-pawn movement
            extend distance dirs = dirs >>= go distance
                    where go dist dir | dist == 0 || outBounds dst || sameColor dst = []
                                      | otherwise = dst:(go (dist - 1) dir)
                                      where dst = plusTuple src dir
            -- castling
            between (r1, c1) (r2, c2) = [(r1, c3) | c3 <- [(1 + min c1 c2)..((-1) + max c1 c2)]]
            castle = [((0, 0), castleBQ, (0, -1)),
                      ((0, 7), castleBK, (0, 1)),
                      ((7, 0), castleWQ, (0, -1)),
                      ((7, 7), castleWK, (0, 1))]
                        >>= \(corner, bool, dir) ->
                                keepTrue [(bool && all emptyp (between src corner), plusTuple src dir)]
        in case at board src of 
                Piece color role -> if color /= turn then [] else case role of
                        Pawn -> case color of
                                     Black -> pawnFwd (1, 0) ++ pawnTake [(-1, -1), (-1, 1)]
                                     White -> pawnFwd (-1, 0) ++ pawnTake [(1, -1), (1, 1)]
                        Rook -> extend 8 (horiz ++ vert)
                        Knight -> extend 1 ljump
                        Bishop -> extend 8 diag
                        King -> extend 1 eightDirs ++ castle
                        Queen -> extend 8 eightDirs
                _ -> []

findCell :: ChessBoard -> ChessCell -> Maybe Coord
findCell board cell = find ((== cell) . snd) (assocs board) >>= (return . fst)

kingCoord :: ChessBoard -> Color -> Coord
kingCoord board color = case findCell board (Piece color King) of
                             Just coord -> coord
                             _ -> (8, 8)

teamp :: Color -> ChessCell -> Bool
teamp teamColor cell = case cell of
                            Piece color role -> color == teamColor
                            _ -> False

teamCoords :: ChessState -> [Coord]
teamCoords state = map fst . filter (teamp (refTurn state) . snd) $ assocs (refBoard state)

allMoves :: ChessState -> [(Coord, Coord)]
allMoves state =
        teamCoords state >>=
                \src -> moves state src >>=
                        \dst -> [(src, dst)]

allMoveDsts :: ChessState -> [Coord]
allMoveDsts state = concat . map (moves state) $ teamCoords state

-- check rules, must move out of check, cannot castle in or out of check
inCheck :: ChessState -> Bool
inCheck state =
        let color = refTurn state
            src = kingCoord (refBoard state) color
            res = find (== src) . allMoveDsts $ state { refTurn = otherColor color }
        in case res of Just _ -> True
                       _ -> False

validMove :: ChessState -> (Coord, Coord) -> Bool
validMove state (src, dst) =
        and [elem dst $ moves state src,
             not . inCheck $ movePiece state (src, dst)]

allValidMoves :: ChessState -> [(Coord, Coord)]
allValidMoves state = filter (validMove state) $ allMoves state

gameOver :: ChessState -> Bool
gameOver = null . allValidMoves

chessState lst = ChessState (array ((0, 0), (7, 7)) lst) ((8, 8), (8, 8)) True True True True White

pawn_row = take 8 $ repeat Pawn
hind_row = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
empty_row = take 8 $ repeat Empty
starting_position = [fmap (Piece Black) hind_row,
                     fmap (Piece Black) pawn_row,
                     empty_row,
                     empty_row,
                     empty_row,
                     empty_row,
                     fmap (Piece White) pawn_row,
                     fmap (Piece White) hind_row]

initialState
    = chessState $ zip (concat [[(i, j) | j <- [0..7]] | i <- [0..7]])
                       (concat starting_position)

-- showCoords :: [Coord] -> [String]
-- showCoords lst = [(chr $ ord 'a' + c):(chr $ ord '0' + 8 - r):[] | (r, c) <- lst]

validCoord :: String -> Bool
validCoord str = length str == 2 && c0 >= 'a' && c0 <= 'h' && c1 >= '1' && c1 <= '8'
        where c0 = str !! 0
              c1 = str !! 1

readCoord :: String -> Coord
readCoord str = (8 - (ord (str !! 1) - (ord '0')),
                 ord (str !! 0) - (ord 'a'))

promptCoords :: IO (String, String)
promptCoords =
        putStrLn "choose piece to move [a-h][1-8]" >>
        getLine >>= \srcLine ->
                putStrLn "choose a dst [a-h][1-8]" >>
                getLine >>= \dstLine ->
                        return (srcLine, dstLine)

promptMove :: ChessState -> IO (Coord, Coord)
promptMove state =
        promptCoords >>= \(src, dst) ->
                let move = (readCoord src, readCoord dst)
                in if validCoord src && validCoord dst && validMove state move
                      then return move
                      else putStr "invalid move\n" >>
                      promptMove state

playGame :: ChessState -> IO ()
playGame state =
        print state >>
        promptMove state >>= \move ->
                playGame $ movePiece state move

main = playGame initialState
