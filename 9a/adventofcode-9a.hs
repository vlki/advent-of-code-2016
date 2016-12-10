import Text.Regex.PCRE
import Data.List
import Data.Maybe
import qualified Debug.Trace as Trace

findSubstring :: Eq a => [a] -> [a] -> Int
findSubstring needle haystack = fromMaybe (-1) $ findIndex (isPrefixOf needle) (tails haystack)

decompress :: String -> String
decompress compressed = decompressed
    where markerMatches = getAllTextMatches $ compressed =~ "\\(\\d+x\\d+\\)" :: [String]
          marker = head markerMatches
          markerIndex = findSubstring marker compressed
          (_, _, _, [markerCharsNumStr, markerTimesStr]) = marker =~ "(\\d+)x(\\d+)" :: (String, String, String, [String])
          markerCharsNum = read markerCharsNumStr :: Int
          markerTimes = read markerTimesStr :: Int
          markerChars = take markerCharsNum $ drop (markerIndex + length marker) compressed
          markerDecompressed = concat $ replicate markerTimes markerChars
          beforeMarker = take markerIndex compressed
          restDecompressed = decompress $ drop (markerIndex + length marker + markerCharsNum) compressed
          decompressed = if (length markerMatches) == 0 then compressed else beforeMarker ++ markerDecompressed ++ restDecompressed

main = do
    input <- getContents
    putStrLn $ show $ length $ decompress $ head $ lines input
