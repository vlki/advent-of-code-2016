import Text.Regex.Posix
import qualified Data.Map as Map
import Data.List
import Data.Char

shiftCipherOnce :: Char -> Char
shiftCipherOnce 'Z' = 'A'
shiftCipherOnce 'z' = 'a'
shiftCipherOnce x = chr ((ord x) + 1)

shiftCipher :: Integer -> Char -> Char
shiftCipher n '-' = ' '
shiftCipher 1 x = shiftCipherOnce x
shiftCipher n x = shiftCipherOnce $ shiftCipher (n-1) x

decryptRoomName :: String -> String
decryptRoomName roomStr = map (\c -> shiftCipher sectorId c) name
    where (name, _, _) = breakRoomStr roomStr
          sectorId = getSectorId roomStr

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

sectorIdOfNorthPoleObjects :: [String] -> Integer
sectorIdOfNorthPoleObjects roomStrs = getSectorId northPoleObjectsRoomStr
    where realRoomStrs = filter isRealRoom roomStrs
          northPoleObjectsRoomStr = head $ filter (\roomStr -> decryptRoomName roomStr == "northpole object storage") realRoomStrs

main = do
    input <- getContents
    putStrLn $ show $ sectorIdOfNorthPoleObjects $ lines input

-- main = do
--     input <- getContents
--     putStrLn $ unlines $ map (\line -> decryptRoomName line) $ filter isRealRoom $ lines input
