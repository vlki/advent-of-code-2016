module Main where

import Text.Regex.PCRE
import qualified Debug.Trace as Trace

parseRange :: String -> (Int, Int)
parseRange rangeStr = (start, end)
    where
        (_, _, _, [startStr, endStr]) = rangeStr =~ "(\\d+)-(\\d+)" :: (String, String, String, [String])
        start = read startStr :: Int
        end = read endStr :: Int

parseRanges :: [String] -> [(Int, Int)]
parseRanges rangeStrs = map parseRange rangeStrs

matchingRanges :: Int -> [(Int, Int)] -> [(Int, Int)]
-- matchingRanges x ranges | Trace.trace (show x) False = undefined
matchingRanges x ranges = filter (\(a, b) -> x >= a && x <= b) ranges

nextNonBlockedIp :: Int -> [(Int, Int)] -> Int
nextNonBlockedIp x ranges
    | length matching == 0 = x
    | otherwise = nextNonBlockedIp (snd (head matching) + 1) ranges
    where
        matching = matchingRanges x ranges

allNonBlockedIps :: [(Int, Int)] -> [Int]
allNonBlockedIps ranges = go 0
    where
        go x
            | nextNonBlockedIp x ranges > 4294967295 = []
            | otherwise = [nextNonBlockedIp x ranges] ++ go ((nextNonBlockedIp x ranges) + 1)

main :: IO ()
main = do
    input <- getContents
    putStrLn $ show $ length $ allNonBlockedIps $ parseRanges $ lines input
