import Text.Regex.PCRE
import Data.List
import Data.Matrix
import qualified Debug.Trace as Trace
import qualified Data.Vector as Vector

type Screen = Matrix Bool
type Pos = (Int, Int)

rectCommand :: Screen -> String -> Screen
rectCommand screen command = foldl (\scr pos -> setElem True pos scr) screen [(i, j) | i <- [1..rectH], j <- [1..rectW]]
    where (_, _, _, [widthStr, heightStr]) = command =~ "rect (\\d+)x(\\d+)" :: (String, String, String, [String])
          rectW = read widthStr :: Int
          rectH = read heightStr :: Int
          screenW = ncols screen
          screenH = nrows screen

rotateColumn :: Int -> Int -> Screen -> Screen
rotateColumn colNum rotateBy screen = mapCol (\x _ -> (tempCol !! ((x - 1 - rotateBy) `mod` length tempCol))) colNum screen
    where tempCol = Vector.toList $ getCol colNum screen

rotateColumnCommand :: Screen -> String -> Screen
rotateColumnCommand screen command = rotateColumn colNum rotateBy screen
    where (_, _, _, [colNumStr, rotateByStr]) = command =~ "rotate column x=(\\d+) by (\\d+)" :: (String, String, String, [String])
          colNum = (read colNumStr :: Int) + 1
          rotateBy = read rotateByStr :: Int

rotateRow :: Int -> Int -> Screen -> Screen
-- rotateRow rowNum rotateBy screen | Trace.trace ("rotateRow " ++ show rowNum ++ " " ++ show rotateBy) False = undefined
rotateRow rowNum rotateBy screen = mapRow (\x _ -> (tempRow !! ((x - 1 - rotateBy) `mod` length tempRow))) rowNum screen
    where tempRow = Vector.toList $ getRow rowNum screen

rotateRowCommand :: Screen -> String -> Screen
rotateRowCommand screen command = rotateRow rowNum rotateBy screen
    where (_, _, _, [rowNumStr, rotateByStr]) = command =~ "rotate row y=(\\d+) by (\\d+)" :: (String, String, String, [String])
          rowNum = (read rowNumStr :: Int) + 1
          rotateBy = read rotateByStr :: Int

applyCommand :: Screen -> String -> Screen
applyCommand screen command
    | isPrefixOf "rect" command = rectCommand screen command
    | isPrefixOf "rotate column" command = rotateColumnCommand screen command
    | isPrefixOf "rotate row" command = rotateRowCommand screen command
    | otherwise = error $ "Unknown command " ++ command

applyCommands :: [String] -> Screen
applyCommands commands = screenEnd
  where screenStart = matrix screenH screenW $ \(_, _) -> False
        screenEnd = foldl (\screen command -> applyCommand screen command) screenStart commands
        screenW = 50
        screenH = 6

prettyScreen :: Screen -> String
prettyScreen screen = concat $ map (\row -> (map (\x -> if x then '#' else '.') row) ++ "\n") screenLists
    where screenLists = toLists screen

main = do
    input <- getContents
    putStrLn $ prettyScreen $ applyCommands $ lines input

-- main = do
--     input <- getContents
--     putStrLn $ show $ applyCommands $ lines input
