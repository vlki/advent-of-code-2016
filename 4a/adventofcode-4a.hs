import Text.Regex.Posix

isRealRoom :: String -> Bool
isRealRoom roomStr = True

getSectorId :: String -> Int
getSectorId roomStr = 0

sumOfRealRoomSectorIds :: [String] -> Int
sumOfRealRoomSectorIds roomStrs = sum (map getSectorId roomStrs)

main = do
    input <- getContents
    putStrLn $ show $ sumOfRealRoomSectorIds $ lines input
