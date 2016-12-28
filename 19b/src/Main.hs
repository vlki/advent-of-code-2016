module Main where

import qualified Debug.Trace as Trace

-- type State = (Int, [Bool])
--
-- initElves :: Int -> [Bool]
-- initElves n = replicate n True
--
-- nextTrueIndex :: Int -> [Bool] -> Int
-- nextTrueIndex index elves
--     | (elves !! next) == True = next
--     | otherwise = nextTrueIndex (index + 1) elves
--     where
--         next = (index + 1) `mod` length elves
--
-- nextSourceIndex :: Int -> [Bool] -> Int
-- nextSourceIndex index elves = iterate nextTrueIndexWithFixedElves index !! nextTimes
--     where
--         nextTimes = trueElvesCount `div` 2
--         trueElvesCount = length $ filter (\x -> x) elves
--         nextTrueIndexWithFixedElves index = nextTrueIndex index elves
--
-- makeTurn :: State -> State
-- makeTurn state@(index, elves) | Trace.trace (show state) False = undefined
-- makeTurn (index, elves)
--     | index == sourceIndex = error "There is only one elf"
--     | otherwise = (nextIndex, nextElves)
--     where
--         sourceIndex = nextSourceIndex index elves
--         nextElves = take sourceIndex elves ++ [False] ++ drop (sourceIndex + 1) elves
--         nextIndex = nextTrueIndex index nextElves
--
-- makeTurnsTillOneElf :: State -> State
-- makeTurnsTillOneElf state@(index, elves)
--     | index == (nextTrueIndex index elves) = state
--     | otherwise = makeTurnsTillOneElf $ makeTurn state
--
-- main :: IO ()
-- main = do
--     let initState = (0, (initElves 730))
--     let endState = makeTurnsTillOneElf initState
--     putStrLn $ show endState


significantRange :: Int -> (Int, Int)
significantRange 0 = (0, 1)
significantRange 1 = (1, 2)
significantRange x = go (2, 4)
    where
        go (a, b)
            | a <= x && b > x = (a, b)
            | otherwise = go (b, b + ((b - a) * 3))

elfNum :: Int -> Int
elfNum x = (x - a) + (if x >= halfRangeNum then x - halfRangeNum + 1 else 0) + 1
    where
        (a, b) = significantRange x
        halfRangeLength = ((b - a) `div` 2)
        halfRangeNum = a + halfRangeLength

main :: IO ()
main = do
    putStrLn $ show $ elfNum 3014387
