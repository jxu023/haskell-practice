import Data.Array
import Data.Char
import Debug.Trace

type Coord = (Int, Int)

data Color = Black | White deriving (Eq)

data Role = Pawn | Rook | Knight | Bishop | King | Queen

-- EnPassant Cells are marked with turn number of creation
-- and treated like Empty
data ChessCell = Empty | EnPassant Int | Piece Color Role

-- TODO use polymorphism over switch/if/case
-- create a ChessMove typeclass ... or look up other OO techs in haskell
-- you'll get much smaller functions with less repetition than if divided by
-- another method such as just creating another function

instance Show ChessCell where
        show Empty = " "
        show (EnPassant t) = show t
        show (Piece color role) =
                case color of
                     White -> case role of
                                   Pawn -> "♙"
                                   Rook -> "♖"
                                   Knight -> "♘"
                                   Bishop -> "♗"
                                   King -> "♔"
                                   Queen -> "♕"
                     Black -> case role of
                                   Pawn -> "♟"
                                   Rook -> "♜"
                                   Knight -> "♞"
                                   Bishop -> "♝"
                                   King -> "♚"
                                   Queen -> "♛"

outBounds (dr, dc) = dr < 0 || dc < 0 || dr > 7 || dc > 7
inBounds = not . outBounds

-- returns a list of valid moves
-- TODO en passant, castling, needs more game context
moves (ChessBoard board) src@(r, c) =
        let dest (dr, dc) = (r + dr, c + dc)
            piecep coord = (inBounds coord &&) $ case (board ! coord) of Piece _ _ -> True
                                                                         _ -> False
            whitep coord = matchWhite $ board ! coord
                where matchWhite (Piece color _) = color == White
            sameColor dst = piecep src && piecep dst && whitep src == whitep dst
            diffColor dst = piecep dst && piecep src && not (sameColor dst)
            emptyp = not . piecep
            hindrowp coord@(r, c) = r == 1 && whitep coord || r == 6 && (not $ whitep coord)

            -- unless blocked, can move 1 fwd and move 2 fwd if on starting row
            pawnFwd dir = let dst = dest dir
                              dst2 = dest dst
                          in if emptyp dst
                                then ([dst] ++) $ if emptyp dst2 && hindrowp src
                                                   then [dst2] else []
                                else []
            -- checks opp piece colors and EnPassant in dest
            pawnTake dirs = dirs >>= \dir ->
                let dst = dest dir
                in if diffColor dst then [dst] else []

            -- dirs for input to extend
            horiz = [(0, 1), (0, -1)]
            vert = [(1, 0), (-1, 0)]
            diag = [(i, j) | i <- [-1, 1], j <- [-1, 1]]
            eightDirs = concat [horiz, vert, diag]
            ljump = [(i, j) | i <- [-1, 1, 2, -2], j <- [-1, 1, 2, -2], abs i /= abs j]
            -- move/take cells pointed by dir within dist, blocked by sameColor
            extend distance dirs = dirs >>= go distance
                    where go dist dir | dist == 0
                                        || outBounds dst
                                        || sameColor dst = []
                                      | otherwise = dst:(go (dist - 1) dir)
                                      where dst = dest dir

            -- king and castle must never moved before and cells between them are empty
            -- for both kinds of castles
            -- should just mark a boolean flag when either have moved inside the ChessGame state
            castle = undefined

        -- TODO apply filter to list if the king is in check such that next moves must avoid check
        in case board ! src of 
                Empty -> []
                Piece color role -> case role of
                        Pawn -> case color of
                                Black -> pawnFwd (1, 0)
                                         ++ pawnTake [(-1, -1), (-1, 1)]
                                White -> pawnFwd (-1, 0)
                                         ++ pawnTake [(1, -1), (1, 1)]
                        Rook -> extend 8 (horiz ++ vert)
                        Knight -> extend 1 ljump
                        Bishop -> extend 8 diag
                        King -> extend 1 eightDirs -- ++ castle -- TODO define castle
                        Queen -> extend 8 eightDirs

data ChessBoard = ChessBoard (Array Coord ChessCell)

-- TODO look into ben lynn's haskell js gui thing
instance Show ChessBoard where
        show (ChessBoard b)
                = let ((br, bc), (er, ec)) = bounds b
                      colCoords = (\x -> "  " ++ x ++ "\n") $
                        concat [ [chr $ c + ord 'a', ' '] | c <- [bc..ec]]
                      rowCoords r row = let r' = 8 - r
                                        in show r' ++ row ++ "|" ++ show r' ++ "\n"
                  in (\board -> colCoords ++ board ++ colCoords) $
                       [br..er] >>= \r -> rowCoords r $
                       [bc..ec] >>= \c -> "|" ++ show (b ! (r, c))

chessBoard lst = ChessBoard $ array ((0, 0), (7, 7)) lst

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
    = chessBoard $ zip (concat [[(i, j) | j <- [0..7]] | i <- [0..7]])
                       (concat starting_position)

-- TODO read games using algebraic notation

-- TODO use the list monad to enumerate possible board states

-- TODO look at real world haskell examples to use Modules correctly
--      then divide up your code into separate files.

--      or use code folding and save those into a session
--     

-- TODO convert coordinates between (Int, Int) and String
queryMoveLoop = do
        putStr . show $ starting_board
        putStr "moves for src: "
        line <- getLine
        let r = 8 - (ord (line !! 1) - (ord '0'))
            c = ord (line !! 0) - (ord 'a')
        trace (show r ++ " " ++ show c ++ "\n") $ print $ moves starting_board (r, c)
        _ <- getChar
        queryMoveLoop

main = queryMoveLoop
