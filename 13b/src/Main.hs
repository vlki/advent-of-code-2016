module Main where

import Data.Matrix
import Text.Regex.PCRE
import qualified Debug.Trace as Trace

type Maze = Matrix Int
type Pos = (Int, Int)

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = toBinary (n `quot` 2) ++ [n `rem` 2]

generateMazeBlock :: (Int, Int) -> Int
generateMazeBlock (my, mx) = if isWall then -2 else -1
    where x = mx - 1
          y = my - 1
          isWall = (sum (toBinary (x*x + 3*x + 2*x*y + y + y*y + 1350)) `mod` 2) == 1

isMazeBlockWall :: Pos -> Maze -> Bool
isMazeBlockWall pos maze = getPosValue pos maze == -2

isMazeBlockNonExpanded :: Pos -> Maze -> Bool
isMazeBlockNonExpanded pos maze = getPosValue pos maze == -1

isValidPos :: Pos -> Maze -> Bool
isValidPos (x, y) maze = x >= 0 && y >= 0 && x < (ncols maze) && y < (nrows maze)

getPosValue :: Pos -> Maze -> Int
getPosValue (x, y) maze = getElem (y + 1) (x + 1) maze

setPosValue :: Pos -> Int -> Maze -> Maze
setPosValue pos@(x, y) value maze = setElem value (y + 1, x + 1) maze

isExpandableBlock :: Pos -> Int -> Maze -> Bool
isExpandableBlock pos potentialValue maze = isValidPos pos maze && not (isMazeBlockWall pos maze) && (isMazeBlockNonExpanded pos maze || (getPosValue pos maze > potentialValue))

generateMaze :: Maze
generateMaze = matrix 60 60 generateMazeBlock

leeExpandWave :: Pos -> Maze -> Maze
leeExpandWave pos@(posX, posY) inMaze = outMaze
    where neighbouring = [(posX - 1, posY), (posX + 1, posY), (posX, posY - 1), (posX, posY + 1)]
          posValue = getPosValue pos inMaze
          expPosValue = posValue + 1
          expandable = filter (\pos -> isExpandableBlock pos expPosValue inMaze) neighbouring
          neighValuesMaze = foldl (\maze expPos -> (setPosValue expPos expPosValue maze)) inMaze expandable
          outMaze = foldl (\maze expPos -> (leeExpandWave expPos maze)) neighValuesMaze expandable

leeRun :: Pos -> Maze -> Maze
leeRun startPos maze = leeExpandWave startPos $ setPosValue startPos 0 maze

countBlocksInStepsLimit :: Pos -> Int -> Maze -> Int
countBlocksInStepsLimit startPos limit maze = length $ filter (\num -> num >= 0 && num <= limit) $ toList $ leeRun startPos maze

main :: IO ()
main = do
    putStrLn $ show $ countBlocksInStepsLimit (1, 1) 50 $ generateMaze
    -- putStrLn $ show $ leeRun (1, 1) $ generateMaze
