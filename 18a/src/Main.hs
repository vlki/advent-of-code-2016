module Main where

isSafe :: Bool -> Bool -> Bool -> Bool
isSafe False False True = False -- Its left and center tiles are traps, but its right tile is not.
isSafe True False False = False -- Its center and right tiles are traps, but its left tile is not.
isSafe False True True = False -- Only its left tile is a trap.
isSafe True True False = False -- Only its right tile is a trap.
isSafe _ _ _ = True

isTileSafe :: Int -> [Bool] -> Bool
isTileSafe i row
    | i < 0 = True
    | i >= length row = True
    | otherwise = row !! i

nextRow :: [Bool] -> [Bool]
nextRow row = map (\(i, tile) -> isSafe (isTileSafe (i - 1) row) (isTileSafe i row) (isTileSafe (i + 1) row)) withIndexes
    where
        withIndexes = zip [0..] row :: [(Int, Bool)]

fromStr :: String -> [Bool]
fromStr = map mapChar
    where
        mapChar '.' = True
        mapChar '^' = False
        mapChar c = error ("Unknown char " ++ [c])

toStr :: [Bool] -> String
toStr = map mapChar
    where
        mapChar True = '.'
        mapChar False = '^'

generateRows :: Int -> [Bool] -> [[Bool]]
generateRows 1 row = [row]
generateRows num row = [row] ++ generateRows (num - 1) (nextRow row)

countSafeTiles :: [[Bool]] -> Int
countSafeTiles = length . filter (\x -> x) . concat

main :: IO ()
main = do
    putStrLn $ show $ countSafeTiles $ generateRows 40 $ fromStr ".^..^....^....^^.^^.^.^^.^.....^.^..^...^^^^^^.^^^^.^.^^^^^^^.^^^^^..^.^^^.^^..^.^^.^....^.^...^^.^."
