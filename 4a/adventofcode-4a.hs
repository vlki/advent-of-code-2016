import Text.Regex.Posix
import qualified Data.Map as Map
import Data.List

compareCharFrequencies :: (Char, Integer) -> (Char, Integer) -> Ordering
compareCharFrequencies (c1, f1) (c2, f2)
    | f1 < f2 = GT
    | f1 > f2 = LT
    | f1 == f2 = compare c1 c2

roomChecksum :: String -> String
roomChecksum roomStr = checksum
    where (name, _, _) = breakRoomStr roomStr
          nameWithoutDashes = filter (\c -> c /= '-') name
          frequencies = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- nameWithoutDashes]
          sortedFrequencies = sortBy compareCharFrequencies frequencies
          fullChecksum = map (\(c, f) -> c) sortedFrequencies
          checksum = take 5 fullChecksum

isRealRoom :: String -> Bool
isRealRoom roomStr = expectedChecksum == reportedChecksum
    where expectedChecksum = roomChecksum roomStr
          (_, _, reportedChecksum) = breakRoomStr roomStr

breakRoomStr :: String -> (String, String, String)
breakRoomStr roomStr = (name, sectorIdStr, checksum)
    where (_, _, _, [name, sectorIdStr, checksum]) = roomStr =~ "([a-z-]+)-([0-9]+)\\[([a-z]+)\\]" :: (String, String, String, [String])

getSectorId :: String -> Integer
getSectorId roomStr = read sectorIdStr
    where (_, sectorIdStr, _) = breakRoomStr roomStr

sumOfRealRoomSectorIds :: [String] -> Integer
sumOfRealRoomSectorIds roomStrs = sum sectorIds
    where sectorIds = map getSectorId realRoomStrs
          realRoomStrs = filter isRealRoom roomStrs

main = do
    input <- getContents
    putStrLn $ show $ sumOfRealRoomSectorIds $ lines input

-- main = do
--     input <- getContents
--     putStrLn $ unlines $ map (\line -> roomChecksum line) $ lines input
