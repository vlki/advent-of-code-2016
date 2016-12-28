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

lowestNonBlockedIp :: [(Int, Int)] -> Int
lowestNonBlockedIp ranges = go 0
    where
        go x
            | length (matchingRanges x ranges) == 0 = x
            | otherwise = go (snd (head (matchingRanges x ranges)) + 1)

main :: IO ()
main = do
    input <- getContents
    putStrLn $ show $ lowestNonBlockedIp $ parseRanges $ lines input
