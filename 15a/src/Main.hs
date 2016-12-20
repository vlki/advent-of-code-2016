module Main where

import Data.List
import qualified Debug.Trace as Trace

type Disc = (Int, Int)

fallsThrough :: [Int] -> [Int] -> Bool
fallsThrough positions numPositions = sum (modLists (addLists [1, 2, 3, 4, 5, 6] positions) numPositions) == 0

addLists :: [Int] -> [Int] -> [Int]
addLists a b = map (\(ax, bx) -> ax + bx) $ zip a b

modLists :: [Int] -> [Int] -> [Int]
modLists a b = map (\(ax, bx) -> ax `mod` bx) $ zip a b

tick :: [Int] -> [Int] -> [Int]
tick positions numPositions = modLists (map (+ 1) positions) numPositions

findFirstFallThrough :: [Int] -> [Int] -> Int -> Int
findFirstFallThrough positions numPositions time | Trace.trace (show time ++ "\t" ++ show positions ++ "\t" ++ show (fallsThrough positions numPositions)) False = undefined
findFirstFallThrough positions numPositions time
    | fallsThrough positions numPositions = time
    | otherwise = findFirstFallThrough (tick positions numPositions) numPositions (time + 1)

main :: IO ()
main = do
    putStrLn $ show $ findFirstFallThrough [15, 2, 4, 2, 2, 0] [17, 3, 19, 13, 7, 5] 0
