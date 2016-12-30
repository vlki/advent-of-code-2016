module Main where

import Data.Array
import Data.List
import Data.Maybe
import Text.Regex.PCRE
import qualified Debug.Trace as Trace

type NodeInfo = (Int, Int)
type NodePos = (Int, Int)
type Grid = Array (Int, Int) NodeInfo
type Node = (NodePos, NodeInfo)

parseInputLine :: String -> (NodePos, NodeInfo)
parseInputLine inputLine = ((x, y), (used, avail))
    where
        (_, _, _, [xStr, yStr, usedStr, availStr]) = inputLine =~ "/dev/grid/node-x(\\d+)-y(\\d+) +\\d+T +(\\d+)T +(\\d+)T" :: (String, String, String, [String])
        x = read xStr :: Int
        y = read yStr :: Int
        used = read usedStr :: Int
        avail = read availStr :: Int

parseInputLines :: [String] -> Grid
parseInputLines inputLines = grid
    where
        inputLinesWithoutHeader = drop 2 inputLines
        parsed = map parseInputLine inputLinesWithoutHeader
        grid = array ((0, 0), (33, 29)) parsed

canCopyToNode :: Node -> Node -> Bool
-- canCopyToNode a b | Trace.trace (show a ++ show b) False = undefined
canCopyToNode (_, (aUsed, aAvail)) (_, (bUsed, bAvail)) = bAvail >= aUsed && aUsed > 0

-- copyTargetNodes :: Grid -> Node -> [Node]
-- copyTargetNodes grid node@(nodePos@(nodePosX, nodePosY), nodeInfo) =
--     (if westNodeExists && canCopyToNode node westNode then [westNode] else []) ++
--     (if eastNodeExists && canCopyToNode node eastNode then [eastNode] else []) ++
--     (if northNodeExists && canCopyToNode node northNode then [northNode] else []) ++
--     (if southNodeExists && canCopyToNode node southNode then [southNode] else [])
--     where
--         westNodeExists = nodePosX - 1 >= 0
--         westNode = ((nodePosX - 1, nodePosY), (grid ! (nodePosX - 1, nodePosY)))
--         eastNodeExists = nodePosX + 1 < (fst . snd . bounds) grid
--         eastNode = ((nodePosX + 1, nodePosY), (grid ! (nodePosX + 1, nodePosY)))
--         northNodeExists = nodePosY - 1 >= 0
--         northNode = ((nodePosX, nodePosY - 1), (grid ! (nodePosX, nodePosY - 1)))
--         southNodeExists = nodePosY + 1 < (snd . snd . bounds) grid
--         southNode = ((nodePosX, nodePosY + 1), (grid ! (nodePosX, nodePosY + 1)))

copyTargetNodes :: Grid -> Node -> [Node]
copyTargetNodes grid node@(nodePos@(nodePosX, nodePosY), nodeInfo) = filter (canCopyToNode node) nodes
    where
        nodes = assocs grid

viablePairs :: Grid -> [(Node, Node)]
viablePairs grid = concat $ map (\n -> map (\tn -> (n, tn)) (copyTargetNodes grid n)) nodes
    where
        nodes = assocs grid

main :: IO ()
main = do
    input <- getContents
    putStrLn $ show $ length $ viablePairs $ parseInputLines $ lines input
