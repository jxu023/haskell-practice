import Data.Array
import Data.Char

data Color = Black | White
           deriving (Show)

data Role = Pawn | Rook | Knight | Bishop | King | Queen

data ChessCell = Empty | Piece Color Role
instance Show ChessCell where
        show Empty = " "
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
                                   Bishop -> "♗"
                                   King -> "♚"
                                   Queen -> "♛"


data ChessBoard = ChessBoard (Array (Int, Int) ChessCell)

-- TODO look into ben lynn's haskell gui thing
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

b = chessBoard [((i, j), Piece Black Knight) | i <- [0..7], j <- [0..7]]

-- TODO add support to read chess notation, and store games
main = do
        putStr . show $ b

