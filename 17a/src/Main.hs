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
    (if posX > 0 && isLeftOpen then [(Left, State (posX - 1, posY) (path ++ "L"))] else []) ++
    (if posX < 3 && isRightOpen then [(Right, State (posX + 1, posY) (path ++ "R"))] else []) ++
    (if posY > 0 && isUpOpen then [(Up, State (posX, posY - 1) (path ++ "U"))] else []) ++
    (if posY < 3 && isDownOpen then [(Down, State (posX, posY + 1) (path ++ "D"))] else [])
    where
        (posX, posY) = pos
        hash = md5 path
        isUpOpen = isDoorOpen (hash !! 0)
        isDownOpen = isDoorOpen ((drop 1 hash) !! 0)
        isLeftOpen = isDoorOpen ((drop 2 hash) !! 0)
        isRightOpen = isDoorOpen ((drop 3 hash) !! 0)

-- data Tree a = Node {
--     rootLabel :: a,
--     subForest :: Forest a
-- }
-- type Forest a = [Tree a]

explore :: State -> [Tree (Move, State)]
explore = map go . moves
    where
        go (label, state) = Node (label, state) (explore state)

breadthFirstSearch :: (a -> Bool) -> [Tree a] -> Maybe [a]
breadthFirstSearch p = combine Seq.empty []
    where
        combine queue ancestors branches =
            go (queue >< (Seq.fromList . map ((,) ancestors) $ branches))
        go queue =
            case viewl queue of
                EmptyL -> Nothing
                (ancestors, Node a bs) :< queued ->
                    if p a
                    then Just . reverse $ a:ancestors
                    else combine queued (a:ancestors) bs

solve :: State -> Maybe [Move]
solve = fmap (map fst) . breadthFirstSearch ((== (3, 3)) . position . snd) . explore

getPath :: Maybe [Move] -> String
getPath (Just a) = map moveChar a
getPath Nothing = "not found"

moveChar :: Move -> Char
moveChar Left = 'L'
moveChar Right = 'R'
moveChar Up = 'U'
moveChar Down = 'D'

main :: IO ()
main = do
    putStrLn $ getPath $ solve $ State (0, 0) "edjrjqaa"
