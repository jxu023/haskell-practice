import Data.Array
import Data.Char
import Data.List

{-
- TODOs
-
-
- divide into modules/files
-   ChessTypes.hs
-   ChessChars.hs
-   ChessRules.hs
-   ChessGame.hs
- refer to ~/rwh/ examples
-
- unit testing
-
- read games using algebraic notation
-}

type Coord = (Int, Int)

data Color = Black | White deriving (Eq)
instance Show Color where
    show Black = "Black"
    show White = "White"

data Role = Pawn | Rook | Knight | Bishop | King | Queen

data ChessCell = Empty
               | Piece {refColor :: Color, refRole :: Role}
               | OutOfBounds

type ChessBoard = Array Coord ChessCell

data ChessState = ChessState { refBoard :: ChessBoard
                             -- (square to move, pawn to take)
                             -- (8,8) for invalid
                             , refEnpassant :: (Coord, Coord)
                             -- True for can castle
                             , refCastleWhiteKside :: Bool
                             , refCastleWhiteQside :: Bool
                             , refCastleBlackKside :: Bool
                             , refCastleBlackQside :: Bool
                             , refTurn :: Color
                             }

-- TODO consider polymorphism over switch/if/case
-- ChessMove typeclass ... or look up other OO techs in haskell

-- TODO look into ben lynn's haskell js gui
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
interleave a b = concat $ transpose [a, b]

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
                 show bq ++ " " ++ show bk,
                 show wq ++ " " ++ show wk,
                 show p]

outBounds (dr, dc) = dr < 0 || dc < 0 || dr > 7 || dc > 7
inBounds = not . outBounds

at board coord | inBounds coord = board ! coord
               | otherwise      = OutOfBounds

keepTrue :: [(Bool, a)] -> [a]
keepTrue [] = []
keepTrue ((bool, x):lst) | bool = x:(keepTrue lst)
                         | otherwise = keepTrue lst

{-
pawnFwd board src dir
        = let psrc = at board src
              (p1, c1) = plusPiece board src dir
              (p2, c2) = plusPiece board c1 dir
              in keepTrue [(emptyp p1, p1),
                           (emptyp p1 && emptyp p2 && pawnrowp src (at board src), p2)]
-}

-- directions that chesspieces can move in
horiz :: [Coord]
horiz = [(0, 1), (0, -1)]
vert = [(1, 0), (-1, 0)]
diag = [(i, j) | i <- [-1, 1], j <- [-1, 1]]
eightDirs = concat [horiz, vert, diag]
ljump = [(i, j) | i <- [-1, 1, 2, -2], j <- [-1, 1, 2, -2], abs i /= abs j]

-- TODO for move execution apply filter to list if the king is in check such that next moves must avoid check
-- returns a list of valid moves
moves :: ChessState -> Coord -> [Coord]
moves (ChessState board passant castleWK castleWQ castleBK castleBQ turn) src =
        let piecep coord = case at board coord of Piece _ _ -> True
                                                  _ -> False
            whitep coord = case at board coord of Piece White _ -> True
                                                  _ -> False
            emptyp coord = case at board coord of Empty -> True
                                                  _ -> False
            sameColor dst = piecep src && piecep dst && whitep src == whitep dst
            diffColor dst = piecep dst && piecep src && not (sameColor dst)
            pawnrowp coord@(r, _) = r == 1 && (not $ whitep coord) || r == 6 && whitep coord
            -- TODO .. make sure colors are not switched
            passantrowp coord@(r, _) = r == 2 && (not $ whitep coord) || r == 5 && whitep coord

            plusTuple (r, c) (dr, dc) = (r + dr, c + dc)

            -- TODO rewrite with keepTrue
            pawnFwd dir = let dst = plusTuple src dir
                              dst2 = plusTuple dst dir
                          in if emptyp dst
                                then ([dst] ++) $ if emptyp dst2 && pawnrowp src
                                                     then [dst2] else []
                                else []
            -- TODO rewrite with keepTrue, check enpassant logic
            pawnTake dirs = dirs >>= \dir ->
                let dst = plusTuple src dir
                in if diffColor dst || fst passant == dst && passantrowp src then [dst] else []

            extend distance dirs = dirs >>= go distance
                    where go dist dir | dist == 0 || outBounds dst || sameColor dst = []
                                      | otherwise = dst:(go (dist - 1) dir)
                                      where dst = plusTuple src dir
            
            between (r1, c1) (r2, c2) = [(r1, c3) | c3 <- [(1 + min c1 c2)..((-1) + max c1 c2)]]
            castle = [((0, 0), castleBQ, (0, -1)),
                      ((0, 7), castleBK, (0, 1)),
                      ((7, 0), castleWQ, (0, -1)),
                      ((7, 7), castleWK, (0, 1))]
                        >>= \(corner, bool, dir) -> if bool && all emptyp (between src corner)
                                                       then [plusTuple src dir]
                                                       else []

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

coords :: [Coord] -> [String]
coords lst = [(chr $ ord 'a' + c):(chr $ ord '0' + 8 - r):[] | (r, c) <- lst]

move :: ChessState -> Coord -> ChessState
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
