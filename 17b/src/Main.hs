module Main where

import Prelude hiding (Left, Right)
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Array
import Data.List
import Data.Tree
import qualified Debug.Trace as Trace
import Data.Sequence (viewl, ViewL (..), (><))
import qualified Data.Sequence as Seq

md5 :: String -> String
md5 str = show $ MD5.md5 $ LB.pack str

isDoorOpen :: Char -> Bool
isDoorOpen c
    | c `elem` ['0'..'9'] = False
    | c == 'a' = False
    | c `elem` ['a'..'f'] = True
    | otherwise = error $ "Unexpected char " ++ [c]

data State = State { position :: (Int, Int), path :: String }
    deriving (Eq, Ord, Show)

data Move = Up | Down | Left | Right
    deriving (Show)

moves :: State -> [(Move, State)]
moves (State pos path) =
    (if posX > 0 && isLeftOpen && isPosNotEnd then [(Left, State (posX - 1, posY) (path ++ "L"))] else []) ++
    (if posX < 3 && isRightOpen && isPosNotEnd then [(Right, State (posX + 1, posY) (path ++ "R"))] else []) ++
    (if posY > 0 && isUpOpen && isPosNotEnd then [(Up, State (posX, posY - 1) (path ++ "U"))] else []) ++
    (if posY < 3 && isDownOpen && isPosNotEnd then [(Down, State (posX, posY + 1) (path ++ "D"))] else [])
    where
        (posX, posY) = pos
        hash = md5 ("edjrjqaa" ++ path)
        isUpOpen = isDoorOpen (hash !! 0)
        isDownOpen = isDoorOpen ((drop 1 hash) !! 0)
        isLeftOpen = isDoorOpen ((drop 2 hash) !! 0)
        isRightOpen = isDoorOpen ((drop 3 hash) !! 0)
        isPosNotEnd = pos /= (3, 3)

explore :: State -> [Tree (Move, State)]
explore = map go . moves
    where
        go (label, state) = Node (label, state) (explore state)

longestPath :: [String] -> String
longestPath = snd . maximum . map (\x -> (length x, x))

preorder :: Forest a -> [a]
preorder [] = []
preorder a = (map (\(Node s _) -> s) a) ++ (concat (map (\(Node _ xs) -> preorder xs) a))

filterToVaultPaths :: [(Move, State)] -> [(Move, State)]
filterToVaultPaths = filter (\(m, s) -> position s == (3, 3))

findLongestPath :: Forest (Move, State) -> String
findLongestPath = longestPath . map (\(m, s) -> path s) . filterToVaultPaths . preorder

main :: IO ()
main = do
    -- putStrLn $ show $ length $ longestPath $ validPaths (State (0, 0) "") []
    putStrLn $ show $ length $ findLongestPath $ explore (State (0, 0) "")
