module Main where

import Data.List
import qualified Debug.Trace as Trace

flipZeroAndOne :: Char -> Char
flipZeroAndOne '0' = '1'
flipZeroAndOne '1' = '0'

flipZeroesAndOnes :: String -> String
flipZeroesAndOnes s = map flipZeroAndOne s

expandUsingDragonCurve :: Int -> String -> String
expandUsingDragonCurve targetLength s
    | length s >= targetLength = take targetLength s
    | otherwise = expandUsingDragonCurve targetLength (s ++ "0" ++ reverse (flipZeroesAndOnes s))

oddEvenSplit :: [a] -> ([a], [a])
oddEvenSplit [] = ([], [])
oddEvenSplit (x:y:xs) = (x:xp, y:yp) where (xp, yp) = oddEvenSplit xs

checksumPair :: (Char, Char) -> Char
checksumPair ('0', '0') = '1'
checksumPair ('1', '1') = '1'
checksumPair ('0', '1') = '0'
checksumPair ('1', '0') = '0'

checksum :: String -> String
checksum s
    | odd (length s) = s
    | otherwise = checksum (map checksumPair (zip odds evens))
    where (odds, evens) = oddEvenSplit s

main :: IO ()
main = do
    putStrLn $ checksum $ expandUsingDragonCurve 35651584 "01111001100111011"
