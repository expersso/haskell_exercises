-- Implementation of Nim, based on Hutton (2016)
import Data.Char
import System.IO

data Player = Player1 | Player2

instance Show Player where
    show Player1 = "Player 1"
    show Player2 = "Player 2"

next :: Player -> Player
next Player1 = Player2
next Player2 = Player1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

finished :: Board -> Bool
finished = all (== 0)

newline :: IO ()
newline = putChar '\n'

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn $ replicate num '*'

putBoard :: Board -> IO ()
putBoard board = putBoard' board 1
    where
        putBoard' [] _ = newline
        putBoard' (x:xs) row = do putRow row x
                                  putBoard' xs (row+1)

getDigit :: String -> IO Int
getDigit prompt = do
    putStr prompt 
    x <- getLine
    newline
    if all isDigit x then
         return (read x)
    else
         do putStrLn "ERROR: Invalid digit."
            getDigit prompt

play :: Board -> Player -> IO ()
play board player = 
    do newline
       putBoard board
       if finished board then
           do newline
              putStrLn $ "Game over. " ++ show (next player) ++ " wins!"
       else
           do newline
              print player
              row <- getDigit "Enter a row number: "
              num <- getDigit "Stars to remove: "
              if valid board row num then
                  play (move board row num) (next player)
              else
                  do putStrLn "ERROR: Invalid move."
                     play board player

nim :: IO ()
nim = play initial Player1

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          nim

