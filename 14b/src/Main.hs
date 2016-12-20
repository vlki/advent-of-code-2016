module Main where

import Data.Function.Memoize
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as LB
import Text.Regex.PCRE
import qualified Debug.Trace as Trace

applyMd5 :: String -> String
applyMd5 str = show $ md5 $ LB.pack str

getSubHash :: Int -> Int -> String -> String
getSubHash index 0 hash = applyMd5 hash
getSubHash index rehashCounter hash = applyMd5 $ getSubHash index (rehashCounter - 1) hash

getHash :: Int -> String
getHash index = getSubHash index 2016 ("zpqevtbw" ++ show index)

getHashMemoized :: Int -> String
getHashMemoized = memoize getHash

hasHashLetterFiveInRow :: Char -> Int -> Int -> Bool
hasHashLetterFiveInRow letter index maxIndex
    | index > maxIndex = False
    | otherwise = hasFiveInRow || hasHashLetterFiveInRow letter (index + 1) maxIndex
    where hash = getHashMemoized index
          hasFiveInRow = hash =~ ("(" ++ [letter] ++ "){5}") :: Bool

isValidHash :: String -> Int -> Bool
isValidHash hash index = hasThreeInRow && hasHashLetterFiveInRow letter (index+1) (index+1000)
    where hasThreeInRow = hash =~ "([0-9abcdef])\\1\\1" :: Bool
          (_, _, _, [letterStr]) = hash =~ "([0-9abcdef])\\1\\1" :: (String, String, String, [String])
          letter = letterStr !! 0

findValidHashes :: Int -> Int -> [(Int, String)]
findValidHashes 0 index = []
findValidHashes num index
    | isValidHash hash index = [(index, hash)] ++ findValidHashes (num - 1) (index + 1)
    | otherwise = findValidHashes num (index + 1)
    where hash = getHashMemoized index

main :: IO ()
main = do
    putStrLn $ show $ findValidHashes 64 0
