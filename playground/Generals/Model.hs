module Model where

import Data.Array

data Player = Red | Blue | None

data Cell = Empty | Mountain | Army Player Int | City Player Int
          deriving (Show)

type Coord = (Int, Int)
type Board = Array Coord Cell

type Turn = Int

-- association list of cities owned by a player
-- first city is the player's general
type Cities = [(Player, [Coord])]

data GameState = GameState Board Turn Cities

initializeRandom board = undefined

emptyBoard rows cols = array ((0, 0), (r, c)) pairs
    where pairs = zip (concat [[(i, j) | j <- [0..c]] | i <- [0..r]])
                      (repeat Empty)
          r = rows - 1
          c = cols - 1

columnIndices board = case bounds board of ((_, c1), (_, c2)) -> [c1..c2]
rowIndices board = case bounds board of ((r1, _), (r2, _)) -> [r1..r2]

instance Show Player where
    show player = case player of Red -> "R"
                                 Blue -> "B"
                                 None -> "N"

showCell board r c = case board ! (r, c) of Empty             -> "_"
                                            Mountain          -> "^"
                                            City player count -> "C" ++ show player ++ show count
                                            Army player count -> show player ++ show count

alignCell width str = str ++ (take (width - (length str)) (repeat ' '))

-- align cells, each with width of 6: city 1, color 1, count 3, space 1
showRow board r = concat $ map (alignCell 6 . showCell board r)
                               (columnIndices board)

showBoard board = unlines $ map (showRow board)
                                (rowIndices board)

initialState = GameState initialBoard 0 [(Red, [(0, 0)]), (Blue, [(4, 9)])]
    where initialBoard = emptyBoard 5 10 // [((0, 0), City Red 1), ((4, 9), City Blue 1)]

instance Show GameState where
    show (GameState board turn _) = showBoard board ++ "\nTurn: " ++ show (div turn 2) ++ "\n"
