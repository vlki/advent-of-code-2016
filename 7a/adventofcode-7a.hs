import Text.Regex.PCRE

doesSequenceContainsAbba :: String -> Bool
doesSequenceContainsAbba ipSequence = ipSequence =~ "([a-z])((?!\\1)[a-z])\\2\\1"

nonHypernetSequences :: String -> [String]
nonHypernetSequences ip = cleanedMatches
    where rawMatches = getAllTextMatches $ ip =~ "[a-z]+\\[|\\][a-z]+\\[|\\][a-z]+" :: [String]
          cleanedMatches = map (\match -> filter (\c -> c /= '[' && c /= ']') match) rawMatches

hypernetSequences :: String -> [String]
hypernetSequences ip = cleanedMatches
    where rawMatches = getAllTextMatches $ ip =~ "\\[[^\\[]+\\]" :: [String]
          cleanedMatches = map (\match -> filter (\c -> c /= '[' && c /= ']') match) rawMatches

isIpWithTls :: String -> Bool
isIpWithTls ip = nonHypernetSequencesContainAbba && not hypernetSequencesContainAbba
    where nonHypernetSequencesContainAbba = or $ map doesSequenceContainsAbba $ nonHypernetSequences ip
          hypernetSequencesContainAbba = or $ map doesSequenceContainsAbba $ hypernetSequences ip

countIpsWithTls :: [String] -> Int
countIpsWithTls ips = length ipsWithTls
    where ipsWithTls = filter isIpWithTls ips

main = do
    input <- getContents
    putStrLn $ show $ countIpsWithTls $ lines input

-- main = do
--     input <- getContents
--     putStrLn $ show $ map (\ip -> isIpWithTls ip) $ lines input

-- main = do
--     input <- getContents
--     putStrLn $ show $ map (\ip -> map doesSequenceContainsAbba $ nonHypernetSequences ip) $ lines input
