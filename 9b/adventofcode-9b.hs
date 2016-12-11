import Text.Regex.PCRE
import Data.List
import Data.Maybe
import Text.Show.Functions
import qualified Debug.Trace as Trace

data NumTree = El Int | Node Int [NumTree] deriving Show

findSubstring :: Eq a => [a] -> [a] -> Int
findSubstring needle haystack = fromMaybe (-1) $ findIndex (isPrefixOf needle) (tails haystack)

splitByMarkers :: String -> [NumTree]
splitByMarkers compressed
    | length markerMatches == 0 = [El (length compressed)]
    | otherwise = [beforeMarkerNumTree, markerNumTree] ++ restNumTrees
    where markerMatches = getAllTextMatches $ compressed =~ "\\(\\d+x\\d+\\)" :: [String]
          marker = head markerMatches
          markerIndex = findSubstring marker compressed
          (_, _, _, [markerCharsNumStr, markerTimesStr]) = marker =~ "(\\d+)x(\\d+)" :: (String, String, String, [String])
          markerCharsNum = read markerCharsNumStr :: Int
          markerTimes = read markerTimesStr :: Int
          markerChars = take markerCharsNum $ drop (markerIndex + length marker) compressed
          beforeMarker = take markerIndex compressed
          beforeMarkerNumTree = El (length beforeMarker)
          markerNumTree = Node markerTimes (splitByMarkers markerChars)
          restStillCompressed = drop (markerIndex + length marker + markerCharsNum) compressed
          restNumTrees = splitByMarkers restStillCompressed

toNumTree :: String -> NumTree
toNumTree compressed = Node 1 $ splitByMarkers compressed

sumNumTree :: NumTree -> Int
sumNumTree (El num) = num
sumNumTree (Node timesNum subNumTrees) = sum $ map (\subNumTree -> timesNum * (sumNumTree subNumTree)) subNumTrees

main = do
    input <- getContents
    putStrLn $ show $ sumNumTree $ toNumTree $ head $ lines input
