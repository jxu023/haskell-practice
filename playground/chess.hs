import Data.Array
import Data.Char
import Data.List
import Debug.Trace

-- TODO divide into modules/files
-- ChessTypes.hs
-- ChessChars.hs
-- ChessRules.hs
-- ChessGame.hs
-- refer to ~/rwh/ examples

type Coord = (Int, Int)

data Color = Black | White deriving (Eq)
instance Show Color where
    show Black = "Black"
    show White = "White"

data Role = Pawn | Rook | Knight | Bishop | King | Queen

-- EnPassant Cells are marked with turn number of creation
-- and treated like Empty
data ChessCell = Empty | Piece Color Role | OutOfBounds

data ChessState = ChessState { refBoard :: Array Coord ChessCell
                             , refEnpassant :: (Coord, Coord)
                             , refCastleWhiteKside :: Bool
                             , refCastleWhiteQside :: Bool
                             , refCastleBlackKside :: Bool
                             , refCastleBlackQside :: Bool
                             , refTurn :: Color
                             }

-- TODO use polymorphism over switch/if/case
-- create a ChessMove typeclass ... or look up other OO techs in haskell
-- you'll get much smaller functions with less repetition than if divided by
-- another method such as just creating another function

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

coordRow :: String
coordRow = ' ':[[' ', c], | c <- ['a'..'h']]

wrapCoordRow x = coordRow ++ "\n" ++ x ++ coordRow ++ "\n"

boardRow -> [Char] -> Int -> String
boardRow cells r = r':[['|', c] | c <- cells ++ [r']]
    where r' = show $ 8 - r

-- TODO look into ben lynn's haskell js gui thing
instance Show ChessState where
    show (ChessState b p wk wq bk bq turn) = wrapCoordRow board
        where board = [br..er] >>= boardRow [pieceChar
    
    let ((br, bc), (er, ec)) = bounds b
              boardLine = rowCoord:cells:'|':
              rowCoords r row = let r' = 8 - r
                                    in show r' ++ row ++ "|" ++ show r' ++ "\n"
                  in (++ (show turn ++ " to move\n\n"))
                     . (\board -> colCoords ++ board ++ colCoords)
                     $ [br..er] >>= \r -> rowCoords r
                     $ [bc..ec] >>= \c -> "|" ++ show (b ! (r, c))

outBounds (dr, dc) = dr < 0 || dc < 0 || dr > 7 || dc > 7
inBounds = not . outBounds

at board coord | inBounds coord = board ! coord
               | otherwise      = OutOfBounds

-- TODO reduce duplication below ... template haskell?
piecep board coord = case at board coord of Piece _ _ -> True
                                            _         -> False

piecep :: ChessCell -> Bool
piecep (Piece _ _) = True
piecep _ = False

whitep :: ChessCell -> Bool
whitep (Piece White _) = True
whitep _ = False

blackp :: ChessCell -> Bool
blackp (Piece Black _) = True
blackp _ = False

emptyp:: ChessCell -> Bool
emptyp Empty = True
emptyp _ = False

teamp :: ChessCell -> ChessCell -> Bool
teamp src dst = piecep src && piecep dst && whitep src == whitep dst
teamp _ _ = False

enemyp :: ChessCell -> ChessCell -> Bool
enemyp src dst = piecep dst && piecep src && whitep src /= whitep dst
enemyp _ _ = False

pawnrowp :: Coord -> ChessCell -> Bool
pawnrowp coord@(r, _) cell = r == 1 && blackp cell || r == 6 && whitep coord

passantrowp :: Coord -> ChessCell -> Bool
passantrowp coord@(r, _) = r == 2 && blackp cell || r == 5 && whitep coord

plusPair :: Num a => (a, a) -> (a, a) -> (a, a)
plusPair (r, c) (dr, dc) = (r + dr, c + dc)

moves :: ChessState -> Coord -> [Coord]
moves (ChessState board passant castleWK castleWQ castleBK castleBQ turn) src =
        let 
            pawnFwd dir = let dst = plusTuple src dir
                              dst2 = plusTuple dst dir
                          in if emptyp dst
                                then ([dst] ++) $ if emptyp dst2 && pawnrowp src
                                                     then [dst2] else []
                                else []
            -- checks opp piece colors and EnPassant in dst
            pawnTake dirs = dirs >>= \dir ->
                let dst = plusTuple src dir
                in if diffColor dst || fst passant == dst && passantrowp src then [dst] else []

            -- dirs for input to extend
            horiz = [(0, 1), (0, -1)]
            vert = [(1, 0), (-1, 0)]
            diag = [(i, j) | i <- [-1, 1], j <- [-1, 1]]
            eightDirs = concat [horiz, vert, diag]
            ljump = [(i, j) | i <- [-1, 1, 2, -2], j <- [-1, 1, 2, -2], abs i /= abs j]
            -- adds cells in direction of dirs until blocked by sameColor
            extend distance dirs = dirs >>= go distance
                    where go dist dir | dist == 0 || outBounds dst || sameColor dst = []
                                      | otherwise = dst:(go (dist - 1) dir)
                                      where dst = plusTuple src dir
            
            -- TODO unit tests
            between (r1, c1) (r2, c2) = [(r1, c3) | c3 <- [(1 + min c1 c2)..((-1) + max c1 c2)]]
            castle = [((0, 0), castleBQ, (0, -1)),
                      ((0, 7), castleBK, (0, 1)),
                      ((7, 0), castleWQ, (0, -1)),
                      ((7, 7), castleWK, (0, 1))]
                        >>= \(corner, bool, dir) -> if bool && all emptyp (between src corner)
                                                       then [plusTuple src dir]
                                                       else []

        -- TODO apply filter to list if the king is in check such that next moves must avoid check
        in case at board src of 
                Piece color role -> if color /= turn then [] else case role of
                        Pawn -> case color of
                                Black -> pawnFwd (1, 0)
                                         ++ pawnTake [(-1, -1), (-1, 1)]
                                White -> pawnFwd (-1, 0)
                                         ++ pawnTake [(1, -1), (1, 1)]
                        Rook -> extend 8 (horiz ++ vert)
                        Knight -> extend 1 ljump
                        Bishop -> extend 8 diag
                        King -> extend 1 eightDirs ++ castle
                        Queen -> extend 8 eightDirs
                _ -> []

chessState lst = ChessState (array ((0, 0), (7, 7)) lst) ((8, 8), (8, 8)) True True True True White

-- construct the starting board position
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

starting_board
    = chessState $ zip (concat [[(i, j) | j <- [0..7]] | i <- [0..7]])
                       (concat starting_position)

-- TODO read games using algebraic notation

-- TODO look at real world haskell examples to use Modules correctly
--      then divide up your code into separate files.

--      or use code folding and save those into a session
--     

coords :: [Coord] -> [String]
coords lst = [(chr $ ord 'a' + c):(chr $ ord '0' + 8 - r):[] | (r, c) <- lst]

move :: ChessState -> Coord -> ChessState
-- move state coord = let board = refBoard state
                   --in 
move = undefined

queryMoveLoop = do
        putStr . show $ starting_board
        putStr "moves for src: "
        line <- getLine
        let r = 8 - (ord (line !! 1) - (ord '0'))
            c = ord (line !! 0) - (ord 'a')
        print . coords $ moves starting_board (r, c)
        queryMoveLoop

main = queryMoveLoop
