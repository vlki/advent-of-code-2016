module Main where

import Data.List
import Data.Maybe
import Text.Regex.PCRE
import qualified Debug.Trace as Trace

swapElemIndexes :: Int -> Int -> [a] -> [a]
swapElemIndexes x y s = take a s ++ [s !! b] ++ drop (a + 1) (take b s) ++ [s !! a] ++ drop (b + 1) s
    where
        a = min x y
        b = max x y

rotateElemsRight :: Int -> [a] -> [a]
rotateElemsRight a xs = drop ((length xs) - am) xs ++ take ((length xs) - am) xs
    where am = a `mod` (length xs)

rotateElemsLeft :: Int -> [a] -> [a]
rotateElemsLeft a xs = drop am xs ++ take am xs
    where am = a `mod` (length xs)

opSwapPosition :: String -> String -> String
opSwapPosition op pwd = swapElemIndexes x y pwd
    where
        (_, _, _, [xStr, yStr]) = op =~ "swap position (\\d+) with position (\\d+)" :: (String, String, String, [String])
        x = read xStr :: Int
        y = read yStr :: Int

opSwapLetter :: String -> String -> String
opSwapLetter op pwd = swapElemIndexes x y pwd
    where
        (_, _, _, [aStr, bStr]) = op =~ "swap letter ([a-z]) with letter ([a-z])" :: (String, String, String, [String])
        a = aStr !! 0
        b = bStr !! 0
        x = fromJust $ elemIndex a pwd
        y = fromJust $ elemIndex b pwd

opRotateBasedOnLetterPosition :: String -> String -> String
opRotateBasedOnLetterPosition op pwd = rotateElemsLeft (rotateNum x) pwd
    where
        (_, _, _, [aStr]) = op =~ "rotate based on position of letter ([a-z])" :: (String, String, String, [String])
        a = aStr !! 0
        x = fromJust $ elemIndex a pwd
        rotateNum 0 = 9
        rotateNum 1 = 1
        rotateNum 2 = 6
        rotateNum 3 = 2
        rotateNum 4 = 7
        rotateNum 5 = 3
        rotateNum 6 = 8
        rotateNum 7 = 4

opRotateLeft :: String -> String -> String
opRotateLeft op pwd = rotateElemsRight x pwd
    where
        (_, _, _, [xStr]) = op =~ "rotate left (\\d+) steps?" :: (String, String, String, [String])
        x = read xStr :: Int

opRotateRight :: String -> String -> String
opRotateRight op pwd = rotateElemsLeft x pwd
    where
        (_, _, _, [xStr]) = op =~ "rotate right (\\d+) steps?" :: (String, String, String, [String])
        x = read xStr :: Int

opReversePositions :: String -> String -> String
opReversePositions op pwd = take x pwd ++ (reverse (drop x (take (y + 1) pwd))) ++ drop (y + 1) pwd
    where
        (_, _, _, [xStr, yStr]) = op =~ "reverse positions (\\d+) through (\\d+)" :: (String, String, String, [String])
        x = read xStr :: Int
        y = read yStr :: Int

opMovePosition :: String -> String -> String
opMovePosition op pwd = take x extractedPwd ++ [extractedChar] ++ drop x extractedPwd
    where
        (_, _, _, [xStr, yStr]) = op =~ "move position (\\d+) to position (\\d+)" :: (String, String, String, [String])
        x = read xStr :: Int
        y = read yStr :: Int
        extractedChar = pwd !! y
        extractedPwd = take y pwd ++ drop (y + 1) pwd

applyScrambleOp :: String -> String -> String
applyScrambleOp pwd op
    | take 13 op == "swap position" = opSwapPosition op pwd
    | take 11 op == "swap letter" = opSwapLetter op pwd
    | take 34 op == "rotate based on position of letter" = opRotateBasedOnLetterPosition op pwd
    | take 11 op == "rotate left" = opRotateLeft op pwd
    | take 12 op == "rotate right" = opRotateRight op pwd
    | take 17 op == "reverse positions" = opReversePositions op pwd
    | take 13 op == "move position" = opMovePosition op pwd
    | otherwise = pwd

applyReversedScrambleOps :: String -> [String] -> String
applyReversedScrambleOps pwd ops = foldl applyScrambleOp pwd (reverse ops)

main :: IO ()
main = do
    input <- getContents
    putStrLn $ applyReversedScrambleOps "fbgdceah" $ lines input
