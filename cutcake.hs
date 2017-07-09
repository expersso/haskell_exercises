-- Implements the so-called Cut Cake game, which is commonly used
-- in combinatorial game theory

module CutCake where

import Data.List.Split (chunksOf)
import Data.List (intersperse, intercalate)

type Pos = Int

data Player = L | R 
  deriving (Eq, Show)

-- L cuts vertically, R cuts horizontally
data Direction = Vertical | Horizontal 
  deriving (Eq, Show)

data Block = Block { rows :: Int
                   , cols :: Int
                   } deriving (Show)

next :: Player -> Player
next L = R
next R = L

-- R wins if L starts turn with no available vertical cuts, and vice versa
winner :: Player -> [Block] -> Maybe Player
winner p bs = if noAvailableCuts then Just (next p) else Nothing
  where noAvailableCuts = sum (map (\x -> f x - 1) bs) == 0
        f               = if p == L then cols else rows

-- Cut a block in a given direction and position
cutAt :: Block -> Direction -> Pos -> [Block]
cutAt b d x 
  | d == Vertical   = [Block (rows b) x, Block (rows b) (cols b - x)]
  | d == Horizontal = [Block x (cols b), Block (rows b - x) (cols b)]

-- Check if a cut is valid
validCut :: Direction -> Block -> Pos -> Bool
validCut d b x
  | d == Vertical   = x > 0 && x <= (ncols - 1)
  | d == Horizontal = x > 0 && x <= (nrows - 1) 
      where nrows = rows b
            ncols = cols b

-- Display a block
putBlock :: Block -> String
putBlock b = intercalate bars verts
  where bars  = "\n" ++ replicate nbars '-' ++ "\n"
        nbars = cols b * 2 - 1
        verts = map (intercalate "|" . map show) xxs
        xxs   = chunksOf (cols b) [1..dim]
        dim   = rows b * cols b

-- Display a list of blocks
putBlocks :: [Block] -> IO ()
putBlocks bs =  putStrLn $ bss ++ "\n"
  where bss = concat $ zipWith (++) bls xs
        bls = map ((++ ")\n\n") . ("\n\nBlock " ++)) is
        is  = map show [0..]
        xs  = map putBlock bs 

validIndex :: [a] -> Pos -> Bool
validIndex xs i = i >= 0 && i < length xs

getPos :: String -> IO Pos
getPos prompt = do
  putStrLn prompt
  fmap read getLine

-- Split a block in the given position, direction, and cut position
-- and return a new list of blocks
cutBlock :: [Block] -> Pos -> Direction -> Pos -> [Block]
cutBlock bbs bp dir cut = take bp bbs ++ bs ++ drop (bp+1) bbs
    where bs = cutAt (bs !! bp) dir cut

-- Drop all 1x1 blocks
dropTrivialBlocks :: [Block] -> [Block]
dropTrivialBlocks = filter (\b -> rows b + cols b > 2) 

cutCake :: Player -> [Block] -> IO ()
cutCake p bbs = case winner p bbs of
  Just x  -> putStrLn $ "Player " ++ show x ++ " wins!"
  Nothing -> do
    putStrLn $ "\nPlayer " ++ show p                    -- Display player
    let bs = dropTrivialBlocks bbs                      -- Drop trivial blocks
    putBlocks bs                                        -- Display blocks

    let dir = if p == L then Vertical else Horizontal   -- Get cut direction
    bp  <- getPos "Select a block: "                    -- Get block position
    cut <- getPos $ "Select a " ++ show dir ++ " cut: " -- Get cut position

    if validIndex bs bp && validCut dir (bs !! bp) cut  -- Check valid pos
      then cutCake (next p) (cutBlock bs bp dir cut)    -- Play next round
      else do
        putStrLn "Invalid position(s)! Try again!"
        cutCake p bbs                                   -- Restart round

main = cutCake L [Block 2 4]     -- Instantiate standard game
