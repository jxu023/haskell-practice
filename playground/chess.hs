import Data.Array
import Data.Char
import Data.List
-- import Debug.Trace

{- TODOs
- 
- runghc chess < game.record
-   do this to run games .. these can also serve as testcases
-   just need to implement validation of the endstate
-   good way to make regression tests
-
-   how about a fuzzer making random moves? and u validate by hand
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

showCoord :: Coord -> String
showCoord (r, c) = if (r, c) /= (8, 8)
                      then [chr $ ord 'a' + c, chr $ ord '0' + 8 - r]
                      else "N/A"

instance Show ChessState where
        show (ChessState b p wk wq bk bq turn)
                = unlines $ coordRow:(boardGrid b) ++
                [coordRow,
                 show turn ++ " to move",
                 "castle " ++ show bq ++ " " ++ show bk,
                 "castle " ++ show wq ++ " " ++ show wk,
                 "Enpassant " ++ showCoord (fst p) ++ ", " ++ showCoord (snd p)]

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

diffTuple (r, c) (dr, dc) = (abs $ r - dr, abs $ c - dc)
midTuple (r, c) (dr, dc) = (div (r + dr) 2, div (c + dc) 2)

kingCastlep board (src, dst) =
        case at board src of Piece _ King -> (== 2) . snd $ diffTuple src dst
                             _ -> False

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
        where srcPiece = at board src
              board' = (board //) $  [(src, Empty), (dst, srcPiece)]
                                     ++ keepTrue [(castlep, (rookDst, Piece turn Rook)),
                                                  (castlep, (rookSrc, Empty)),
                                                  (enPassantp, (snd passant, Empty))]
              enPassantp = dst == fst passant && case at board src of Piece _ Pawn -> True
                                                                      _ -> False
              passant' = head $ keepTrue [(passantp, (midTuple src dst, dst)),
                                          (True, ((8, 8), (8, 8)))]
              passantp = case at board src of Piece _ Pawn -> (== 2) . fst $ diffTuple src dst 
                                              _ -> False
              wk' = and [wk, not $ rookMoved (7, 7), not $ kingMoved White]
              wq' = and [wq, not $ rookMoved (7, 0), not $ kingMoved White]
              bk' = and [bk, not $ rookMoved (0, 7), not $ kingMoved Black]
              bq' = and [bq, not $ rookMoved (0, 0), not $ kingMoved Black]
              rookMoved coord = case at board src of Piece _ Rook -> coord == src
                                                     _ -> False
              kingMoved color = case at board src of Piece c King -> color == c
                                                     _ -> False
              castlep = kingCastlep board (src, dst)
              (rookSrc, rookDst) = head $ keepTrue [(white && right, ((7, 7), (7, 5))),
                                                    (white && left,  ((7, 0), (7, 3))),
                                                    (black && right, ((0, 7), (0, 5))),
                                                    (black && left,  ((0, 0), (0, 3)))]
              black = turn == Black
              white = turn == White
              left = snd dst < 4
              right = snd dst > 4

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
            piecesp dst = piecep src && piecep dst
            sameColor dst = piecesp dst && whitep src == whitep dst
            diffColor dst = piecesp dst && whitep src /= whitep dst
            pawnrowp coord@(r, _) = r == 1 && blackp coord || r == 6 && whitep coord
            -- pawn movement
            pawnFwd dir = let dst = plusTuple src dir
                              dst2 = plusTuple dst dir
                          in keepTrue [(emptyp dst, dst),
                                       (emptyp dst && emptyp dst2 && pawnrowp src, dst2)]
            pawnAtk dirs = dirs >>= \dir ->
                    let dst = plusTuple src dir
                    in keepTrue [(diffColor dst || (fst passant == dst), dst)]
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
                                     Black -> pawnFwd (1, 0) ++ pawnAtk [(1, -1), (1, 1)]
                                     White -> pawnFwd (-1, 0) ++ pawnAtk [(-1, -1), (-1, 1)]
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
allMoves state = teamCoords state >>=
                    \src -> moves state src >>=
                            \dst -> [(src, dst)]

allMoveDsts :: ChessState -> [Coord]
allMoveDsts state = concat . map (moves state) $ teamCoords state

inCheck :: ChessState -> Bool
inCheck state =
        let color = refTurn state
            src = kingCoord (refBoard state) color
            res = find (== src) . allMoveDsts $ state { refTurn = otherColor color }
        in case res of Just x -> True
                       _ -> False

validMove :: ChessState -> (Coord, Coord) -> Bool
validMove state (src, dst) =
        and [elem dst $ moves state src,
             not . inCheck $ (movePiece state (src, dst)) { refTurn = refTurn state },
             not $ inCheck state && kingCastlep (refBoard state) (src, dst)]

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

validCoord :: String -> Bool
validCoord str = length str == 2 && c0 >= 'a' && c0 <= 'h' && c1 >= '1' && c1 <= '8'
        where c0 = str !! 0
              c1 = str !! 1

readCoord :: String -> Coord
readCoord str = (8 - (ord (str !! 1) - (ord '0')),
                 ord (str !! 0) - (ord 'a'))

promptCoords :: IO (String, String)
promptCoords =
        putStr "your move [a-h][1-8][a-h][1-8]: " >>
        getLine >>= \line -> return (take 2 line, drop 2 line)

promptMove :: ChessState -> IO (Coord, Coord)
promptMove state =
        promptCoords >>= \(src, dst) ->
                let move = (readCoord src, readCoord dst)
                in if validCoord src && validCoord dst && validMove state move
                      then return move
                      else putStr ("invalid move|" ++ (\(a, b) -> showCoord a ++ ", " ++ showCoord b) move ++ "|\n") >>
                           print (keepTrue [(validCoord src, concat . map showCoord $ moves state (fst move)), (True, "")]) >>
                           promptMove state

playGame :: ChessState -> IO ()
playGame state =
        print state >>
        if gameOver state then putStr "game over!"
                          else promptMove state >>= \move ->
                                  playGame $ movePiece state move

main = playGame initialState
