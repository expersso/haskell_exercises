--hangman, from Hutton (2016)
import System.IO

-- Prevent input character from being echoed to screen
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

-- Print '-' instead of character when inputting word
secretGetString :: IO String
secretGetString = do
    x <- getCh
    if x == '\n' then
        do putChar x
           return []
    else
        do putChar '-'
           xs <- secretGetString
           return (x:xs)

-- Indicate correctly guessed letters
match :: String -> String -> String
match guess word = [if x `elem` guess then x else '-' | x <- word]

play :: String -> IO ()
play word = do
   putStr "? "
   guess <- getLine
   if guess == word then
       putStrLn "You guessed correctly!"
    else
       do putStrLn $ match guess word
          play word

hangman :: IO ()
hangman = do
    putStrLn "Think of a word"
    word <- secretGetString
    putStrLn "Guess a word"
    play word

main = do hSetBuffering stdout NoBuffering
          hangman
