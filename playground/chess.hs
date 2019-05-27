import Data.Array
import Data.Char

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

moves (ChessBoard board) src@(r, c) =
        let dest (dr, dc) = (r + dr, c + dc)
            sameColor dst = case board ! src of
                                 Piece colorS _ -> case board ! dst of
                                                        Piece colorD _ ->
                                                                colorS == colorD
                                                        _ -> False
                                 _ -> False
            pawnFwd = undefined
            pawnTake = undefined -- checks for Enpassant and offending piece color to take

            -- dirs serving as input to extend
            horiz = [(0, 1), (0, -1)]
            vert = [(1, 0), (-1, 0)]
            diag = [(1, 1), (-1, 1), (1, -1), (-1, -1)]
            eightDirs = concat [horiz, vert, diag]
            -- move/take cells pointed by dir within dist, blocked by sameColor
            extend distance dirs = dirs >>= go distance
                    where go dist dir | dist == 0 || sameColor (dest dir) = []
                                      | otherwise = (dest dir):(go (dist - 1) dir)

            ljump = undefined
            castle = undefined

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
                        King -> extend 1 eightDirs ++ castle
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

main = do
        putStr . show $ starting_board

