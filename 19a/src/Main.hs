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
-- makeTurn :: State -> State
-- makeTurn (index, elves) | Trace.trace (show index) False = undefined
-- makeTurn (index, elves)
--     | index == sourceIndex = error "There is only one elf"
--     | otherwise = (nextIndex, nextElves)
--     where
--         sourceIndex = nextTrueIndex index elves
--         nextElves = take sourceIndex elves ++ [False] ++ drop (sourceIndex + 1) elves
--         nextIndex = nextTrueIndex sourceIndex nextElves
--
-- makeTurnsTillOneElf :: State -> State
-- makeTurnsTillOneElf state@(index, elves)
--     | index == (nextTrueIndex index elves) = state
--     | otherwise = makeTurnsTillOneElf $ makeTurn state

highestTwoMultiplier :: Int -> Int
highestTwoMultiplier n = multiply 1
    where
        multiply x
            | x > n = x `div` 2
            | otherwise = multiply (x * 2)

elfNum :: Int -> Int
elfNum n = ((n - (highestTwoMultiplier n)) * 2) + 1

main :: IO ()
main = do
    putStrLn $ show $ elfNum 3014387
