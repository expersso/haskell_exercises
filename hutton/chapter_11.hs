-- Chapter 11 - Unbeatable tic-tac-toe
import Data.List (transpose)

-- Game logic
--- Types for player and grid

data Player = O | B | X deriving (Eq, Ord, Show)

type Grid = [[Player]]

size :: Int
size = 3

empty :: Grid
empty = replicate size (replicate size B)

test2 :: Grid
test2 = [[O,O,O],[O,X,X],[B,B,X]]

test :: Grid
test = [[O,B,X],[O,X,X],[B,B,X]]

--- Checking turns and switching players

next :: Player -> Player
next O = X
next X = O
next B = B -- For completeness

turn :: Grid -> Player
turn g = if os <= xs then O else X
    where
        os = length $ filter (== O) ps
        xs = length $ filter (== X) ps
        ps = concat g

--- Checking end-states

full :: Grid -> Bool
full g = B `notElem` concat g

wins :: Player -> Grid -> Bool
wins p g = any fullLine (rows ++ cols ++ diags)
    where
        fullLine = all (== p)
        rows     = g
        cols     = transpose g
        diags    = [diag g, diag (map reverse g)]
        diag g   = [g !! i !! i | i <- [0..(size-1)]]

won :: Grid -> Bool
won g = wins O g || wins X g

--- Making moves


-- IO
--- Displaying grid

showPlayer :: Player -> [String]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]
showPlayer O = ["   ", " O ", "   "]

interleave :: a -> [a] -> [a]
interleave _ []     = []
interleave _ [x]    = [x]
interleave x (y:ys) = y : x : interleave x ys

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
    where
        beside = foldr1 (zipWith (++))
        bar    = replicate 3 "|"

putGrid :: Grid -> IO ()
putGrid = putStr . unlines . concat . interleave bar . map showRow
    where
        bar = [replicate ((size * 4) - 1) '-']

--- Running game
